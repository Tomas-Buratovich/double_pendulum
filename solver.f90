
!------------------------------------------------------------------------!
!    This subroutine implements the fourth order Runge-Kutta method      !
!                 to solve a system of four ODEs.                        !
!------------------------------------------------------------------------!


MODULE solver

CONTAINS


SUBROUTINE rk4(g,l1,l2,m1,m2,m,y,dydx,n,x,h,yout,derivs)

  IMPLICIT NONE

  !Variable declaration
  INTEGER, INTENT(IN) :: n
  REAL(kind=8), INTENT(IN) :: x, h, dydx(n), y(n)
  REAL(kind=8), INTENT(INOUT) :: yout(n)
  EXTERNAL :: derivs
  integer :: i
  real(kind=8) :: h6, hh, xh, dym(n), dyt(n), yt(n)
  REAL(8), INTENT(IN) :: g,l1,l2,m1,m2,m


  hh = h*0.5d0
  h6 = h/6.0d0
  xh = x + hh


  do i = 1, n
    yt(i) = y(i) + hh*dydx(i)
  end do
  call derivatives(n,g,l1,l2,m1,m2,m,xh, yt, dyt)


  do i = 1, n
    yt(i) = y(i) + hh*dyt(i)
  end do
  call derivatives(n,g,l1,l2,m1,m2,m,xh, yt, dym)

  do i = 1, n
    yt(i) = y(i) + h*dym(i)
    dym(i) = dyt(i) + dym(i)
  end do
  call derivatives(n,g,l1,l2,m1,m2,m,x + h, yt, dyt)

  do i = 1, n
    yout(i) = y(i) + h6*(dydx(i) + dyt(i) + 2.0d0*dym(i))
  end do

  return
end subroutine

END MODULE
