Subroutine Flow
Use Parameters

        If (Fu(i-1,j) > 0.0) Then 
        alphaw=1.0 
        end if
        If (Fu(i+1,j) > 0.0) Then 
        alphae=1.0 
        end if
        If (Fv(i,j-1) > 0.0) Then 
        alphas=1.0 
        end if
        If (Fv(i,j+1) > 0.0) Then 
        alphan=1.0 
        end if
        
End Subroutine Flow        