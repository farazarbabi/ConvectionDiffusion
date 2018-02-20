Subroutine UpWind
Use Parameters

!  ************** Up-Wind Scheme ***********


  
     Do j=2,n-1
      Do i=2,m-1
        
        aw(i,j)= Du(i-1,j) + max(Fu(i-1,j),0.0)
        ae(i,j)= Du(i+1,j) + max(0.0,-Fu(i+1,j))
        an(i,j)= Dv(i,j+1) + max(0.0,-Fv(i,j+1))
        as(i,j)= Dv(i,j-1) + max(Fv(i,j-1),0.0)
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + (Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1)) 
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) )/ap(i,j)
        
     End Do
    End Do
    
End subroutine UpWind