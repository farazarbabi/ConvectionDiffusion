MODULE Parameters


    ! Variables
    INTEGER::m ! number of grids in x direction= m/2				 
	INTEGER::n  ! number of grids in y direction= n/2	
	INTEGER::i,j  
	
	Real(8),Allocatable::Pold(:,:)
	Real(8),Allocatable::P(:,:)
	Real(8),Allocatable::Deltax(:,:)
	Real(8),Allocatable::Deltay(:,:)
	Real(8),Allocatable::Du(:,:)
	Real(8),Allocatable::Dv(:,:)
	Real(8),Allocatable::Fu(:,:)
	Real(8),Allocatable::Fv(:,:)
	Real(8),Allocatable::C(:,:), Cp(:,:), Ct(:,:), Cb(:,:), Ab(:,:), A(:,:), ap(:,:), aw(:,:), ae(:,:), as(:,:), an(:,:), bn(:,:), bw(:,:)
	Real(8),Allocatable::ass(:,:), aww(:,:), ann(:,:), aee(:,:)

	

	DOUBLEPRECISION::alphae, alphaw, alphan, alphas
	
	DOUBLEPRECISION                     :: dx			      !Step in X direction
	DOUBLEPRECISION                     :: dy                  !Step in Y Direction
	DOUBLEPRECISION                     :: Lx, Ly , gamma , Pw, Pe, II , Ru, Ps, Pn, u ,v              		                    
	

	Real(8),Allocatable::X(:),X1(:), Y1(:),Y(:),Diff(:)

	INTEGER::Counter

    DOUBLEPRECISION::DiffP, Counterp
    
	DOUBLEPRECISION::Residual 
    DOUBLEPRECISION::Phi0
	DOUBLEPRECISION::relax
	DOUBLEPRECISION::h	                    !Conv factor
	Character (LEN=2) ::  SS, CD, UP, QU
	Character (LEN=1) ::  G
	Character (LEN=20) ::  fname
	Character (LEN=20) ::  error


    END MODULE Parameters