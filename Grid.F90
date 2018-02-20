Subroutine Grid
Use Parameters
! ********* Grid Generation *************
    
    dx=Lx/(m-1)
    dy=Ly/(n-1)
    
!    Print *, 'Enter U for Uniform grid or N for non-uniform (case sensitive)'
!    Read *, G
    
    G="U"
    if (G.eq.'U') then 
   
    X(1)=0.0
    Y(1)=0.0
    
    do i=2,m
      X(i)=X(i-1)+dx
    end do
    
    do j=2,n
      Y(j)=Y(j-1)+dy
    end do	
    
    Else If (G.eq.'N') then 
     
    X1(1)=0.0
    Y1(1)=0.0
    
    do i=2,m
      X1(i)=X1(i-1)+dx
    end do
    
    do j=2,n
      Y1(j)=Y1(j-1)+dy
    end do	

    do i=1,m/2
      X(i)=2*(X1(i)**2)
    end do 

    do i=(m/2)+1,m
      X(i)= -2*(X1(i)**2) + 4*X1(i)-1
    end do 
    
    do j=1,n/2
      Y(j)= 2*(Y1(j)**2)
    end do	 

    do j=(n/2)+1,n
      Y(j)= -2*(Y1(j)**2) + 4*Y1(j)-1
    end do   
  
  End If

  
    


! **************** Print the grids ******************

	  Open(1,file='grid.plt')
		Write(1,*) 'VARIABLES=X,Y'
		Write(1,*) 'ZONE I=',m,'J=',n,'F=POINT'
	
	    do j=1,n
			do i=1,m
			
				Write(1,'(9f9.3)') X(i),Y(j)	
			
			end do
		end do
		
      close (1)   

! ************* Describing the distances between nodes **************    	
	i=1
	 do j=2,n-1
	    
	    Deltax(i,j)=0.5*(X(i+1)-X(i))
	    Deltay(i,j)=Y(j+1)-Y(j)
	    
	 end do	 
	 
	i=m
	 do j=2,n-1
	    
	    Deltax(i,j)=0.5*(X(i)-X(i-1))
	    Deltay(i,j)=Y(j+1)-Y(j)
	    
	 end do	 	 
	
	j=1
	 do i=2,m-1
	    
	    Deltax(i,j)=X(i+1)-X(i)
	    Deltay(i,j)=0.5*(Y(j+1)-Y(j))
	    
	 end do	 

	j=n
	 do i=2,m-1
	    
	    Deltax(i,j)=X(i+1)-X(i)
	    Deltay(i,j)=0.5*(Y(j)-Y(j-1))
	    
	 end do	

! Corners
        
	    Deltax(1,1)=0.5*(X(2)-X(1))
	    Deltax(1,n)=0.5*(X(2)-X(1))
	    Deltax(m,1)=0.5*(X(m)-X(m-1))
	    Deltax(m,n)=0.5*(X(m)-X(m-1))
	    Deltay(1,1)=0.5*(Y(2)-Y(1))
	    Deltay(1,n)=0.5*(Y(n)-Y(n-1))	    
	    Deltay(m,1)=0.5*(Y(2)-Y(1))	    
	    Deltay(m,n)=0.5*(Y(n)-Y(n-1))	    	    

! General Points
	do i=2,m-1
	 do j=2,n-1
	    
	    Deltax(i,j)=X(i+1)-X(i)
	    Deltay(i,j)=Y(j+1)-Y(j)
	    
	 end do
	end do   	
	
	
End Subroutine Grid