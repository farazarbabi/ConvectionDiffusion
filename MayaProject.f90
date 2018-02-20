!  Assignment3.f90 
!
!  FUNCTIONS:
!  Assignment3 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Assignment3
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Assignment3
    use parameters
    implicit none

    Call Variables
    
! ************** Allocatables ***********

    Allocate(C(m,n), Cp(m,n), Ct(m,n), Cb(m,n), Ab(m,n), A(m,n), X(m), X1(m) , Y(n), Y1(n), Deltax(m,n), Deltay(m,n), Fu(m,n), Fv(m,n), Du(m,n), Dv(m,n) )   
    Allocate(ap(m,n), aw(m,n), ae(m,n), as(m,n), an(m,n), bn(m,n), bw(m,n), ass(m,n), aww(m,n), ann(m,n), aee(m,n))          
    Allocate (P(m,n), Pold(m,n), Diff(1000000))

!************** Body of Assignment3****************
!**************************************************

    Call Grid

!! ********** Initial Values ********

   ap=0.0
   aw=0.0
   as=0.0
   an=0.0
   ae=0.0
   ann=0.0
   aww=0.0
   ass=0.0
   aee=0.0
   
   P= 0.5*Pw
   Pold=0.0

  counter=0

    
!Boundary Conditions

 !Left Wall
   i=1
   Do j=2,n
   
    P(i,j)=Pw
    
   End Do
   
 !Left Down Corner  
   P(1,1)=0.5*Pw
  
 !Right Wall 
   i=m
   Do j=1,n-1
   
    P(i,j)=Pe
    
   End Do 
 
 !Right Up Corner 
   P(m,n)=0.5*Pn
   
 !Down Wall
   j=1
   Do i=2,m-1
   
    P(i,j)=Ps
   
   End Do 

 ! Up Wall
   j=n
   Do i=2,m-1
   
    P(i,j)=Pn
   
   End Do
   
 !*********** Diffusion and Convection Factors *********

    Call Factors 


!********* Scheme Selection *************

!    Print *, 'Now you have to choose your favorite scheme for solution'
!    Print *, 'Enter CD for Central Difference or UP for Upwind or QU for Quick (case sensitive)'
!    Read *, SS
    SS="UP"


!********* Iteration **********

 ! Keeping old values 
 
10    Pold=P

! *********** Final Solution **********
    
    If (SS.eq.'CD') then 
    
    Call Central
    
    Else If (SS.eq.'UP') then 

    Call UpWind
    
    Else If (SS.eq.'QU') then 
    
    Call Quick
   
    End If


    
 !  ************* Convergence Check **********
 
       DiffP=0.0
       counterP=0.0
       
       DO i=2,m-1
		DO j=2,n-1
			
				DiffP=ABS(P(i,j)-Pold(i,j))
				counterP=counterP+DiffP
			
		END DO
	   END DO

	DiffP=counterP
 
      Counter=Counter+1  
     
!      IF	(Counter<=100)  THEN		 	

       IF	(DiffP>Residual) THEN

	  Diff(Counter)=DiffP
 
    If (MOD(Counter,100).eq.0.0) Then
  	Write (*,*) counter, DiffP
  	end if
  	
		GOTO 10
		
	END IF
 
 
 ! Results
   
       ! Write(fname,'(2a,i3.3,a,f3.1,a)')'output-',G,m,SS,gamma,'.plt'
       
    !   Open(2,file=fname)
        Open(2,file='result.plt')
        Write(2,*) 'TITLE ="','ITERATION=',COUNTER,'"'
		Write(2,*) 'VARIABLES=X,Y,P'
		Write(2,*) 'ZONE I=',m,'J=',n,'F=POINT'
	
       do j=1,n
	     do i=1,m
	          
				Write(2,'(2f15.5,f15.4)') X(i),Y(j),P(i,j)

			end do
		end do

! Error History

      !  Write(error,'(2a,i3.3,a,f3.1,a)')'error-',G,m,SS,gamma,'.plt'
        Open(3,file='error.plt')
        Write(3,*) 'TITLE ="',m,SS,gamma,'"'
		Write(3,*) 'VARIABLES=i,DiffP'
		Write(3,*) 'ZONE I=',(counter-2),'J=',1,'F=POINT'
		
		    do i=1,counter-2
            
                II=i
            
				Write(3,'(2f15.7)') II, Diff(i)
	
			end do
			
		 close (3)	

    Print *,'Completed. Press Enter to Exit'
    read *

    end program Assignment3

