program test

  implicit none

  call test_init( )

contains

  !============================================================================
  ! Tests for the init function
  subroutine test_init( )
    use wavelength_grid, only: wavelength_grid_init, nwave, wc

    character(len=*), parameter :: wlgridfile = './data/wavelength_grid.nc'
    integer :: errflg
    character(len=444) :: errmsg

    call wavelength_grid_init( wlgridfile, errmsg, errflg )
    if (errflg/=0) then
       write(*,*) 'FAILURE: '//trim(errmsg)
       call abort()
    end if

    write(*,*) 'nwave : ',nwave
    write(*,*) 'wc : ',wc(:nwave)
    write(*,*) 'Test completed successfully'
  end subroutine test_init

end program test
