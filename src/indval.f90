      subroutine duleg(veg,numplt,numspc,class,clstab,          &
            numcls,numitr,relfrq,relabu,indval,pval,            &
            indcls,maxcls,tmpfrq,tmpabu,pclass,tclass,errcod)

!* passed in

      double precision veg(numplt,numspc)
      integer numplt
      integer numspc
      integer class(numplt)
      integer clstab(numcls)
      integer numcls
      integer numitr

!* passed back

      double precision relfrq(numspc,numcls)
      double precision relabu(numspc,numcls)
      double precision indval(numspc,numcls)
      double precision pval(numspc)
      double precision indcls(numspc)
      integer maxcls(numspc)
      integer errcod

!* relfrq, relabu, and indval all initialized to zero by R

!* scratch

      double precision tmpfrq(numcls)
      double precision tmpabu(numcls)
      integer pclass(numplt)
      integer tclass(numplt)

!* local 

      double precision tmpind
      double precision maxval
      double precision tmpcls
      double precision totveg
      double precision sumrab
      double precision tmpsum


!*********************************** one *********************************

      errcod = 0

      do i=1,numspc
        totveg = 0
        do j=1,numplt
          if (veg(j,i) .gt. 0) then
            totveg = totveg + veg(j,i)
            relabu(i,class(j)) = relabu(i,class(j)) + veg(j,i)
            relfrq(i,class(j)) = relfrq(i,class(j)) + 1
          endif
        end do     

        sumrab = 0.0
        do j=1,numcls
          relabu(i,j) = relabu(i,j) / clstab(j)
          sumrab = sumrab + relabu(i,j)
          relfrq(i,j) = relfrq(i,j) / clstab(j)
        end do   

        maxcls(i) = 0
        maxval = 0

        do j=1,numcls
          relabu(i,j) = relabu(i,j) / sumrab
          indval(i,j) = relabu(i,j) * relfrq(i,j)
          if (indval(i,j) .gt. maxval) then
            maxcls(i) = j
            maxval = indval(i,j)
          endif
        end do      
        indcls(i) = maxval
        if (maxcls(i) .lt. 1 .or. maxcls(i) .gt. numcls) errcod = 1
      end do      

!*********************************** two ************************************

      do i=1,numspc
        if (maxcls(i) .lt. 1 .or. maxcls(i) .gt. numcls) then
          pval(i) = 0
          cycle
        end if
        do j=1,numitr-1
          tmpcls = 0
          tmpind = 0
          totveg = 0
          maxval = 0
          call permute(class,pclass,numplt,tclass)
          do k=1,numcls 
            tmpfrq(k) = 0
            tmpabu(k) = 0
          end do    

          do k=1,numplt
            if (veg(k,i) .gt. 0) then
              totveg = totveg + veg(k,i)
              tmpabu(pclass(k)) = tmpabu(pclass(k)) + veg(k,i)
              tmpfrq(pclass(k)) = tmpfrq(pclass(k)) + 1
            endif
          end do    

          tmpsum = 0.0

          do k=1,numcls
            tmpabu(k) = tmpabu(k) / clstab(k)
            tmpsum = tmpsum + tmpabu(k)
            tmpfrq(k) = tmpfrq(k) / clstab(k)
          end do   

          do k=1,numcls
            tmpabu(k) = tmpabu(k) / tmpsum
            tmpind = tmpabu(k) * tmpfrq(k)
            if (tmpind .gt. maxval) then
              maxval = tmpind
            endif
          end do  
          if (maxval - indval(i,maxcls(i)) .gt. -0.0001) then
            pval(i) = pval(i) + 1
          endif
        end do    
        pval(i) = (pval(i)+1) / numitr
      end do     

      return

      end

!************************************************************

      subroutine permute(class,pclass,numplt,tclass)

      integer class(numplt)
      integer pclass(numplt)
      integer numplt

      double precision unifrnd

!* local

      integer tclass(numplt)
      integer pool

      call rndstart()

      pool = numplt

      do i=1,numplt
        tclass(i) = class(i)
      end do   

      do i=1,numplt
        index = unifrnd()*pool+1
        pclass(i) = tclass(index)
        tclass(index) = tclass(pool)
        pool = pool - 1
      end do   

      call rndend() 

      return

      end
