      subroutine vegtrans(x,y,a,b,numplt,numspc,numval)
c
c* passed in
c
	double precision x(numplt,numspc)
	double precision a(numval)
	double precision b(numval)
	integer numplt
	integer numspc
	integer numval
c
c* passed out
c
 	double precision y(numplt,numspc)
c
c********************* one **************************************
c
      do 10 i=1,numplt
        do 11 j=1,numspc
	if (x(i,j) .eq. 0.0) then
          y(i,j) = 0.0
        else
          do 12 k=1,numval
          if (x(i,j) .eq. a(k)) then
            y(i,j) = b(k)
            goto 11
          endif
   12     continue
        endif
   11   continue
   10 continue
c
      return
c
      end
