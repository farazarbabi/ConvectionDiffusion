Subroutine Quick
Use Parameters

!  ************** Quick Scheme ***********

    alphaw=0.0
    alphae=0.0
    alphan=0.0
    alphas=0.0

! North Wall

    j=n-1
        Do i=3,m-2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= 0.0 
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) + aww(i,j)*P(i-2,j) + aee(i,j)*P(i+2,j) + ass(i,j)*P(i,j-2))/ap(i,j)
        
        End Do
        
! South Wall

    j=2
        Do i=3,m-2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= 0.0 
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) + aww(i,j)*P(i-2,j) + aee(i,j)*P(i+2,j) + ann(i,j)*P(i,j+2) )/ap(i,j)
        
        End Do

! West Wall

    i=2
    Do j=3,n-2

    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= 0.0 
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1)  + aee(i,j)*P(i+2,j) + ann(i,j)*P(i,j+2) + ass(i,j)*P(i,j-2))/ap(i,j)
        
        End Do


! East Wall

    i=m-1
    Do j=3,n-2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= 0.0 
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) + aww(i,j)*P(i-2,j) + ann(i,j)*P(i,j+2) + ass(i,j)*P(i,j-2))/ap(i,j)
        
        End Do

  
 ! East-North Point
  
      j=n-1
      i=m-1
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= 0.0 
        ann(i,j)= 0.0 
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) + aww(i,j)*P(i-2,j) + ass(i,j)*P(i,j-2))/ap(i,j)
        
  ! West-North point
  
      j=n-1
      i=2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= 0.0 
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= 0.0 
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1)  + aee(i,j)*P(i+2,j) + ass(i,j)*P(i,j-2))/ap(i,j)
        
 ! West-South point
 
     j=2
     i=2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= 0.0 
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= 0.0 
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) +  aee(i,j)*P(i+2,j) + ann(i,j)*P(i,j+2)  )/ap(i,j)
        
 ! East-South point
 
     j=2
     i=m-1
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= 0.0 
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= 0.0 
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
!        relax=((1-Al)*ap(i,j)/Al)*Pold(i,j)
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1)  + ann(i,j)*P(i,j+2) + aww(i,j)*P(i-2,j) )/ap(i,j)


!General Nodes

    Do j=3,n-2
        Do i=3,m-2
        
    Call Flow
        
        aw(i,j)=Du(i-1,j) + 6.0*alphaw*Fu(i-1,j)/8.0 + alphae*Fu(i+1,j)/8.0 + 3.0*(1-alphaw)*Fu(i-1,j)/8.0
        ae(i,j)=Du(i+1,j) - 3.0*alphae*Fu(i+1,j)/8.0 - 6.0*(1-alphae)*Fu(i+1,j)/8.0 - (1-alphaw)*Fu(i-1,j)/8.0
        an(i,j)=Dv(i,j+1) - 3.0*alphan*Fv(i,j+1)/8.0 - 6.0*(1-alphan)*Fv(i,j+1)/8.0 - (1-alphas)*Fv(i,j-1)/8.0
        as(i,j)=Dv(i,j-1) + 6.0*alphas*Fv(i,j-1)/8.0 + alphan*Fv(i,j+1)/8.0 + 3.0*(1-alphas)*Fv(i,j-1)/8.0
        
        aww(i,j)= - alphaw*Du(i-1,j)/8.0
        aee(i,j)= (1-alphae)*Fu(i+1,j)/8.0
        ann(i,j)= (1-alphan)*Fv(i,j+1)/8.0
        ass(i,j)= - alphas*Dv(i,j-1)/8.0
        
        ap(i,j)= aw(i,j) + ae(i,j) + an(i,j) + as(i,j) + aww(i,j) + aee(i,j) + ann(i,j) + ass(i,j) +( Fu(i+1,j) - Fu(i-1,j) + Fv(i,j+1) - Fv(i,j-1) )
        
        P(i,j) = (aw(i,j)*P(i-1,j) + ae(i,j)*P(i+1,j) + an(i,j)*P(i,j+1) + as(i,j)*P(i,j-1) + aww(i,j)*P(i-2,j) + aee(i,j)*P(i+2,j) + ann(i,j)*P(i,j+2) + ass(i,j)*P(i,j-2))/ap(i,j)
        
        End Do
    End Do

   
End subroutine Quick