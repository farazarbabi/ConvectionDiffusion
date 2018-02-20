Subroutine Factors
Use Parameters
Call Grid
Call Variables
!   ! F: Convection Factor	For 'U' momentum equation
!   ! D: Diffusion Factor	For 'U' momentum equation


!  **************General Nodes**********
    Do j=1,n
        Do i=1,m
        
        Fu(i,j)=Ru*u*Deltay(i,j)
        Du(i,j)=gamma*Deltay(i,j)/Deltax(i,j)
        
        Fv(i,j)=Ru*v*Deltax(i,j)
        Dv(i,j)=gamma*Deltax(i,j)/Deltay(i,j)
        
        End Do
    End Do

    
End Subroutine Factors