      subroutine npmmds(dist,pos,numplt,numdim,maxitr,bias,stress,
     +                  mindel,maxdel)
c
c* passed in and out
c
      double precision dist(numplt,numplt)
      double precision pos(numplt,numdim)
      double precision stress
      double precision bias
      double precision maxdel
      double precision mindel
      integer numplt
      integer numdim
      integer maxitr
c
c* passed within
c
      double precision dhat(numplt,numplt)
      double precision delta(numplt,numdim)
      integer flag
c
c* local
c
      integer itrnum
      double precision oldstr
c
c* npmmds ***************** one **********************************
c
      call cdhat(dhat,pos,numplt,numdim)
      write(6,'('' Initial stress and bias '')')
      call cstres(dist,dhat,numplt,stress,bias)
      write(6,'('' '')') 
      oldstr = stress
c
      itrnum = 0
   10 itrnum = itrnum + 1
      call zdelta(delta,numplt,numdim)
      call cdelta(dhat,dist,pos,delta,mindel,maxdel,
     +                  numplt,numdim,flag)
      if (flag .eq. 0) then
        goto 14
      endif
c
       do 12 i=1,numplt
        do 13 j=1,numdim
        pos(i,j) = pos(i,j) + delta(i,j)
   13   continue
   12 continue
c
      if (itrnum .lt. maxitr) then
        call cdhat(dhat,pos,numplt,numdim)
        call cstres(dist,dhat,numplt,stress,bias)
        if (stress .gt. oldstr) then
          maxdel = maxdel / 2
c         oldstr = stress
          do 18 i=1,numplt
            do 19 j=1,numdim
            pos(i,j) = pos(i,j) - delta(i,j)
   19       continue
   18     continue
          goto 10
        else if (oldstr-stress .gt. mindel) then
          write(6,'(3f10.4)') stress,bias,maxdel
          oldstr = stress
          goto 10
        else
          oldstr = stress
          maxdel = maxdel / 2
        endif
      endif
c
   14 write(6,'('' Finished in '',i4,'' iterations '')') itrnum
      write(6,'('' Final error = +/- '',f8.5)') stress
c
      do 15 i=2,numplt
        do 16 j=1,i-1
        dhat(i,j) = dhat(j,i)
   16   continue
   15 continue
c
      return
c
      end
c
c* npmmds ***************** subroutine cdhat ********************
c
      subroutine cdhat(dhat,pos,numplt,numdim)
c
      double precision dhat(numplt,numplt)
      double precision pos(numplt,numdim)
      integer numplt
      integer numdim
c
c* cdhat ************** one *************************************
c
      do 10 i=1,numplt-1
        do 11 j=i+1,numplt
        dhat(i,j) = 0.0
          do 12 k=1,numdim
          dhat(i,j) = dhat(i,j) + (pos(i,k) - pos(j,k))**2
   12     continue
        dhat(i,j) = sqrt(dhat(i,j))
   11   continue
   10 continue
c
      return
c
      end
c
c* npmmds ******************** subroutine cdelta *******************
c
      subroutine cdelta(dhat,dist,pos,delta,mindel,maxdel,
     +                  numplt,numdim,flag)
c
      double precision dhat(numplt,numplt)
      double precision dist(numplt,numplt)
      double precision pos(numplt,numdim) 
      double precision delta(numplt,numdim)
      double precision mindel
      double precision maxdel
      integer numplt
      integer numdim
      integer flag
c
c* local
c
      double precision dstar
      double precision diff
      double precision adjust
      double precision mdelta(numdim)
      double precision delmax
c
c* cdelta ****************** one *********************************
c
      flag = 0
      do 10 i=1,numplt-1
        do 11 j=i+1,numplt
        if (abs(dhat(i,j)) .lt. 0.0001) then
          dstar = dist(i,j)
        else
          dstar = dist(i,j) / dhat(i,j)
        endif
          do 12 k=1,numdim
          diff = abs(pos(i,k) - pos(j,k))
          adjust = ((dstar * diff) - diff) * 0.5
          if (abs(adjust) .gt. mindel) then
            flag = 1
            if (pos(i,k) .gt. pos(j,k)) then
              delta(i,k) = delta(i,k) + adjust
              delta(j,k) = delta(j,k) - adjust
            else
              delta(i,k) = delta(i,k) - adjust
              delta(j,k) = delta(j,k) + adjust 
            endif
          endif
   12     continue
   11   continue
   10 continue
c
      do 15 i=1,numplt
        do 16 j=1,numdim
        if (delta(i,j) .lt. 0.0) then
          delta(i,j) = max(delta(i,j),-1.0*maxdel)
        else
          delta(i,j) = min(delta(i,j),maxdel)
        endif
   16   continue
   15 continue
c
      return
c
      end
c
c* npmmds ****************** subroutine zdelta **********************
c
      subroutine zdelta(delta,numplt,numdim)
c
      double precision delta(numplt,numdim)
      integer numplt
      integer numdim
c
      do 10 i=1,numplt
        do 11 j=1,numdim
        delta(i,j) = 0.0
   11   continue
   10 continue
c
      return
c
      end
c
c* npmmds **************** subroutine cstres **************************
c
      subroutine cstres(dist,dhat,numplt,stress,bias)
c
      double precision dist(numplt,numplt)
      double precision dhat(numplt,numplt)
      integer numplt
      double precision stress
      double precision bias
c
      stress = 0.0
      bias = 0.0
c
      do 10 i=1,numplt-1
        do 11 j=i+1,numplt
c       stress = stress + abs(1.0 - (dist(i,j)/dhat(i,j)))
        stress = stress + abs(dist(i,j)-dhat(i,j))
c       stress = stress + abs(dist(i,j)-dhat(i,j)) / dist(i,j)
        bias = bias + (dhat(i,j)-dist(i,j))
   11   continue
   10 continue
c
      stress = stress / ((numplt**2 - numplt) / 2)
      bias = bias / ((numplt**2 - numplt) / 2)
c
      return
c
      end
c
