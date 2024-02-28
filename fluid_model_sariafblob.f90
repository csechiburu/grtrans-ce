  module fluid_model_sariafblob
	
  !use phys_constants, only: pi 
  use fluid_model_sariaf, only: sariaf_vals, init_sariaf
  use fluid_model_blob, only: blob_vals, init_blob
	
  implicit none
  ! sariaf global variables
  real :: riaf_pnth, riaf_n0, riaf_t0, riaf_nnth0, riaf_beta
  integer :: riaf_bl06
  ! blob global variables
  real :: blob_r_c, blob_theta_c, blob_phi_c
  real :: blob_n_e, blob_T_e, blob_B, blob_r_b
  real :: blob_xc, blob_yc, blob_zc	
  
  interface init_sariafblob
    module procedure init_sariafblob
  end interface

  interface del_sariafblob
    module procedure del_sariafblob
  end interface
	
  contains

    subroutine init_sariafblob(n0,t0,nnth0,pnth,beta,bl06,r_c,theta_c,phi_c,n_e,T_e,B,r_b)
		! Sariaf variables
        real, intent(in), optional :: n0,t0,nnth0,pnth,beta
        integer, intent(in), optional :: bl06
		! Blob variables
		real,intent(in),optional :: r_c,theta_c,phi_c,n_e,T_e,B,r_b
		! Initialize
		call init_sariaf(n0,t0,nnth0,pnth,beta,bl06)
		riaf_n0=n0
		riaf_t0=t0
		riaf_nnth0=nnth0
		riaf_pnth=pnth
		riaf_beta=beta
		riaf_bl06=bl06
		call init_blob(r_c,theta_c,phi_c,n_e,T_e,B,r_b)
		blob_r_c=r_c
		blob_theta_c=theta_c
		blob_phi_c=phi_c
		blob_n_e=n_e
		blob_T_e=T_e
		blob_B=B
		blob_r_b=r_b
		write(*,*) '############ inside sariafblob ############'
		write(*,*) 'sariaf input parameters: ', riaf_pnth,riaf_n0,riaf_t0,riaf_nnth0,riaf_beta
		write(*,*) 'blob input parameters: ', blob_r_c, blob_theta_c, blob_phi_c, blob_n_e, blob_T_e, blob_B, blob_r_b
    end subroutine init_sariafblob
    
!     subroutine sariafblob_vals(a,mu,riaf_u,r,theta,phi,n_e,T_e,B_b,sariafblob_n,sariafblob_te,sariafblob_b)
! 		! Sariaf variables
!         real, intent(in) :: a
!         real, dimension(:), intent(in) :: riaf_u,mu
!         real, dimension(size(riaf_u)) :: riaf_r, riaf_rs,riaf_z,riaf_a2
!         real :: riaf_a
!         real, dimension(size(mu)) :: riaf_mu
!         real, dimension(size(riaf_u)) :: riaf_neth, riaf_te, riaf_B, riaf_vr, riaf_omega, &
!              riaf_vth, riaf_nenth
!         integer :: bl06
! 		! Blob variables
!         real,intent(in), dimension(:) :: r, theta, phi
!         real, dimension(size(r)) :: blob_r, blob_theta, blob_phi
! 		real, dimension(size(r)) :: blob_x, blob_y, blob_z, blob_rb2, blob_r2
!         real, intent(in) :: n_e, T_e, B_b
! 		real, dimension(size(r)) :: n_e_arr, T_e_arr, B_arr,weights
! 		! Outputs
! 		real, dimension(size(r)), intent(out) :: sariafblob_n, sariafblob_te, sariafblob_b
!
! 		call sariaf_vals(a,mu,riaf_u,riaf_neth,riaf_te,riaf_B,riaf_vr,riaf_vth,riaf_omega, &
!          riaf_nenth,bl06)
! 		call blob_vals(r,theta,phi,n_e,T_e,B_b,n_e_arr,T_e_arr,B_arr,weights)
!
! 		sariafblob_n = riaf_neth+n_e_arr
! 		sariafblob_te = riaf_te+T_e_arr
! 		sariafblob_b = riaf_B+B_arr
!
!     end subroutine sariafblob_vals

    subroutine del_sariafblob
    end subroutine del_sariafblob
		
  end module fluid_model_sariafblob
		 
		 
		 
			
			