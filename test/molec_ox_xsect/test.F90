program test
  use phot_kind_mod, only: r8 => kind_phot
  use environ_conditions_mod, only: environ_conditions_create, environ_conditions
  use molec_ox_xsect, only: molec_ox_xsect_init
  use molec_ox_xsect, only: molec_ox_xsect_run
  use params_mod, only: input_data_root
  use wavelength_grid, only: wavelength_grid_init, nwave, wc, wl
  use module_xsections, only: rdxs_init

  implicit none

  integer :: nlayers
  character(len=*), parameter :: inputfile='data/MusicBox_env_cond_1col_c190109.nc'

  type(environ_conditions),pointer :: envConds => null()
  real,parameter :: env_conds_lat=10.
  real,parameter :: env_conds_lon=120.

  real(r8) :: zenith
  real(r8), allocatable :: temp(:)
  real(r8), allocatable :: alt(:)
  real(r8), allocatable :: press_mid(:)
  real(r8), allocatable :: press_int(:)

  real(r8), allocatable :: o2vmrcol(:)
  real(r8), allocatable :: srb_o2_xs(:,:)
  real(r8), allocatable :: dto2(:,:)

  integer :: errflg
  character(len=444) :: errmsg
  character(len=*), parameter :: wlgridfile = './data/wavelength_grid.nc'
  write(*,*) 'BEGIN TEST'

  call wavelength_grid_init( wlgridfile, errmsg, errflg )
  if (errflg/=0) then
     write(*,*) 'FAILURE: '//trim(errmsg)
     call abort()
  end if

  input_data_root = './data'

  call rdxs_init( nwave, wl, errmsg, errflg )
  if (errflg/=0) then
     write(*,*) 'FAILURE: '//trim(errmsg)
     call abort()
  end if

  envConds => environ_conditions_create( inputfile, lat=env_conds_lat, lon=env_conds_lon )
  nlayers = envConds%nlayers()

  write(*,*) ' nlayers = ',nlayers

  allocate(temp(nlayers))
  allocate(alt(nlayers))
  allocate(press_mid(nlayers))
  allocate(press_int(nlayers+1))

  allocate(o2vmrcol(nlayers))
  allocate(srb_o2_xs(nwave,nlayers), dto2(nlayers,nwave))

  alt(:nlayers) = envConds%getcol('Z3',nlayers) ! meters
  temp(:nlayers) = envConds%getcol('T',nlayers)
  o2vmrcol(:nlayers) = envConds%getcol('O2',nlayers)

  zenith = envConds%getsrf('SZA')
  press_mid(:nlayers) = envConds%press_mid(nlayers)
  press_int(:nlayers+1) = envConds%press_int(nlayers+1)

  call molec_ox_xsect_init( errmsg, errflg )
  if (errflg/=0) then
      write(*,*) 'FAILURE: '//trim(errmsg)
     call abort()
  end if

  call molec_ox_xsect_run( nlayers, zenith, alt, temp, press_mid, press_int(1), o2vmrcol, dto2, srb_o2_xs, errmsg, errflg )
  if (errflg/=0) then
     write(*,*) 'FAILURE: '//trim(errmsg)
     call abort()
  end if

  write(*,*) 'min / max dto2 : ',minval(dto2),maxval(dto2)
  write(*,*) 'min / max srb_o2_xs : ',minval(srb_o2_xs),maxval(srb_o2_xs)
  
  deallocate(temp)
  deallocate(alt)
  deallocate(press_mid)
  deallocate(press_int)
  deallocate(o2vmrcol)
  deallocate(srb_o2_xs)
  deallocate(dto2)

  write(*,*) 'END TEST'

end program test
