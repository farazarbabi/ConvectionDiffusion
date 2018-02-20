Subroutine Variables
! *************************Describing Varables ******************
!    PRINT *, '********** Assignment No. 3 ***********'
!    PRINT *, '******** Code By FARAZ ARBABI ***************'
!    PRINT *, '---------------------------------------------------------------' 
!    Print *,'ENTER the Dimension of Nodes in X direction (m):'
!    Read *,m
!    Print *,'ENTER the Dimension of Nodes in Y direction (n):'
!    Read *,n
!    Print *,'Enter Diffusion Coefficient (Gamma):'
!    Read *,gamma
!    Print *,'Enter the Temp at West Wall:'
!    Read *,Pw
!    Print *,'Enter the Temp at East Wall:'
!    Read *,Pe
!    Print *,'Enter the Temp at North Wall:'
!    Read *,Pn
!    Print *,'Enter the Temp at South Wall:'
!    Read *,Ps
!    Print *,'Enter the Velocity in X Direction (u):'
!    Read *,u
!    Print *,'Enter the Velocity in Y Direction (v):'
!    Read *,v


 Use Parameters
! Grid cells   
    m=50
    n=50
! Density
	Ru=100.0
! Phi
	Pw= 100.0
	Pe= 0.0
	Ps=0.0
	Pn=100.0
!Velocity
	u=2.0
	v=2.0

! Diffusivity
	gamma=5.0
! residuals
	Residual= 1e-5
! Length
    Lx=1.0
    Ly=1.0
    
    
    End Subroutine Variables
