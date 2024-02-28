  module fluid_model_koral3dblob
	
  !use phys_constants, only: pi 
  use fluid_model_koral3d, only: koral3d_vals, initialize_koral3d_model ! quizas tambien init_koral3d_data???
  use fluid_model_blob, only: blob_vals, init_blob
	
  implicit none
  ! koral3d global variables
  real :: relgammamin=1., relgammamax=1.
  integer :: nrelbin=0
  real(8) :: scalefac
  integer :: nt, indf
  character(len=100) :: dfile, hfile
  logical :: doingkoralnth
  ! blob global variables
  real :: blob_r_c, blob_theta_c, blob_phi_c
  real :: blob_n_e, blob_T_e, blob_B, blob_r_b
  real :: blob_xc, blob_yc, blob_zc	
  
  interface init_koral3dblob
    module procedure init_koral3dblob
  end interface

  interface del_koral3dblob
    module procedure del_koral3dblob
  end interface
	
  contains

    subroutine init_koral3dblob(a,ifile,doingnth,df,hf,ntt,indft,sfac,nrelbin_bk,relgammamin_bk, &
		relgammamax_bk,r_c,theta_c,phi_c,n_e,T_e,B,r_b)
		! Blob variables
		real,intent(in),optional :: r_c,theta_c,phi_c,n_e,T_e,B,r_b
		! koral3d variables
        real(kind=8), intent(in) :: a
        integer,intent(out),optional :: nrelbin_bk
        real,intent(out),optional :: relgammamin_bk, relgammamax_bk
        !integer :: nx, status, nhead
        logical, intent(in) :: doingnth
        real(8),intent(in),optional :: sfac
        character(len=20), intent(in), optional :: ifile
        character(len=20) :: default_ifile='koral.in'
        character(len=100), intent(in), optional :: df,hf
        integer, intent(in), optional :: ntt,indft
		write(*,*) '############ inside koral3dblob ############'
		! Initialize
		call initialize_koral3d_model(a,ifile,doingnth,df,hf,ntt,indft,sfac,nrelbin_bk,relgammamin_bk,relgammamax_bk)
        dfile = df
        hfile = hf
        nt = ntt
        indf = indft
		scalefac=sfac
		doingkoralnth = doingnth
        nrelbin_bk = nrelbin
        relgammamin_bk = relgammamin
        relgammamax_bk = relgammamax
		! check if i need to call all the oher subroutines inside initialize_koral3d_model???
		! also compare with input parameters in my pgrtrans call
		call init_blob(r_c,theta_c,phi_c,n_e,T_e,B,r_b)
		blob_r_c=r_c
		blob_theta_c=theta_c
		blob_phi_c=phi_c
		blob_n_e=n_e
		blob_T_e=T_e
		blob_B=B
		blob_r_b=r_b
		write(*,*) 'blob input parameters: ', blob_r_c, blob_theta_c, blob_phi_c, blob_n_e, blob_T_e, blob_B, blob_r_b
		write(*,*) '############ end of koral3dblob ############'
    end subroutine init_koral3dblob
   

    subroutine del_koral3dblob
    end subroutine del_koral3dblob
		
  end module fluid_model_koral3dblob
	