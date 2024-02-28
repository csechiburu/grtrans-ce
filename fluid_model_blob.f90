  module fluid_model_blob
	
  !use phys_constants, only: pi 
	
  implicit none
  !real, dimension(:) :: blob_r, blob_theta, blob_phi		 	! your position in the sphere
  real :: blob_r_c, blob_theta_c, blob_phi_c 		! position of the center of the blob
  real :: blob_n_e, blob_T_e, blob_B, blob_r_b 		! Blob properties (density, temperature, magnetic field, radius)
  !real, dimension(:) :: blob_x, blob_y, blob_z, blob_rb2, blob_r2		! Blob coordinates in cartesian
  real :: blob_xc, blob_yc, blob_zc				 	! Blob's center in cartesian
  
  interface init_blob								! Specifies blob properties
    module procedure init_blob
  end interface
	
  interface blob_vals								! Specifies model general properties		
    module procedure blob_vals 
  end interface

  interface del_blob
    module procedure del_blob
  end interface
	
  contains

    subroutine init_blob(r_c,theta_c,phi_c,n_e,T_e,B,r_b)
      real,intent(in),optional :: r_c,theta_c,phi_c,n_e,T_e,B,r_b	! Inputs by user (blob center position and physical properties)
      write(*,*) 'init blob: ',present(r_c),present(theta_c),present(phi_c),&
           present(n_e),present(T_e),present(B),present(r_b)
      ! doing this in case we want to add default values later
	  ! here i'm stating the input variables
	  if(present(r_c)) then
      	blob_r_c = r_c
      	blob_theta_c = theta_c
      	blob_phi_c = phi_c
      	blob_n_e = n_e 
      	blob_T_e = T_e
      	blob_B = B
      	blob_r_b = r_b
	  endif
      !write(*,*) 'Fluid model blob inputs: ', blob_r_c, blob_theta_c, blob_phi_c, blob_n_e, blob_T_e, blob_B, blob_r_b

    end subroutine init_blob
    
    subroutine blob_vals(r,theta,phi,n_e,T_e,B_b,n_e_arr,T_e_arr,B_arr,weights) ! changed to n_e, T_e and B_d to distinguish from the input values above
	!Subroutine tells us whether we're inside the blob or not. If yes, it returns its properties.
	 
      real,intent(in), dimension(:) :: r, theta, phi    		! Arrays of positions. Dimension will be specified within grtrans
      real, dimension(size(r)) :: blob_r, blob_theta, blob_phi	! Your position position in space, same as above
	  real, dimension(size(r)) :: blob_x, blob_y, blob_z, blob_rb2, blob_r2	! Coordinates in cartesian. Size is = to position array
      real, intent(in) :: n_e, T_e, B_b
	  real, dimension(size(r)), intent(out) :: n_e_arr, T_e_arr, B_arr, weights ! weights is used when combining with sariaf
	  integer :: i
      !write(*,*) 'blob_r_c = ', blob_r_c
      !write(*,*) 'blob_r_b = ', blob_r_b

      blob_r = r
      blob_theta = theta
      blob_phi = phi
      ! Note that blob's center position, size and physical properties are already passed since they're global variables			

      if (1.0>0.0) then        !blob_r_c > blob_r_b     ! Confirm that blob is further away than its size !
      ! Spherical to cartesian
         blob_x = blob_r*sin(blob_theta)*cos(blob_phi)
         blob_y = blob_r*sin(blob_theta)*sin(blob_phi)
         blob_z = blob_r*cos(blob_theta)
         blob_xc = blob_r_c*sin(blob_theta_c)*cos(blob_phi_c)
         blob_yc = blob_r_c*sin(blob_theta_c)*sin(blob_phi_c)
         blob_zc = blob_r_c*cos(blob_theta_c)
		 ! Checking if point x,y,z satisfies equation of sphere
         blob_r2 = (blob_x-blob_xc)**2 + (blob_y-blob_yc)**2 + (blob_z-blob_zc)**2
         ! Define the region inside the blob
		 blob_rb2 = blob_r_b**2

         where(blob_r2 <= blob_rb2)      ! are we inside the blob or not? If so, assign blob properties
            n_e_arr = blob_n_e
            T_e_arr = blob_T_e
            B_arr = blob_B
			weights = 1d0
         ! If not inside the blob, all properties are zero
         elsewhere
            n_e_arr = 0.0
            T_e_arr = 0.0
            B_arr = 0.0
			weights = 0d0
         endwhere
		 !write(*,*) 'after blob vals:'
		 !write(*,*) 'n_e_arr: ', n_e_arr
		 !write(*,*) 'T_e_arr: ', T_e_arr
		 !write(*,*) 'B_arr: ', B_arr
      else
         write(*,*) 'Your blob is too close!'
      end if
	  !write(*,*) 'weights: ',weights
    end subroutine blob_vals
	
    subroutine del_blob
      ! not sure what to do here?
    end subroutine del_blob
		
  end module fluid_model_blob
		 
		 
		 
			
			