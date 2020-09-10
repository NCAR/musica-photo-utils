program test
  use environ_conditions_mod, only: environ_conditions_create, environ_conditions

  implicit none

  integer :: nlayers
  character(len=*), parameter :: inputfile='data/MusicBox_env_cond_1col_c190109.nc'

  type(environ_conditions),pointer :: envConds => null()
  real,parameter :: env_conds_lat=10.
  real,parameter :: env_conds_lon=120.

  real :: zenith
  real, allocatable :: temp(:)

  write(*,*) 'BEGIN TEST'

  envConds => environ_conditions_create( inputfile, lat=env_conds_lat, lon=env_conds_lon )
  nlayers = envConds%nlayers()

  write(*,*) ' nlayers = ',nlayers

  allocate(temp(nlayers))
  temp(:nlayers) = envConds%getcol('T',nlayers)

  zenith = envConds%getsrf('SZA')

  write(*,*) ' solar zenith = ',zenith
  write(*,*) ' temperature : ',temp

  deallocate(temp)

  write(*,*) 'END TEST'

end program test
