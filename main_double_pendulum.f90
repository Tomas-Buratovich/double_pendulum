
!--------------------------------------------------------------------------------------------!
!                                    DOUBLE PENDULUM                                         !
!--------------------------------------------------------------------------------------------!
!   main program double pendulum: Solves de set of ODEs using Runge-Kutta 4th. order method  !                    
!                                                                                            !
!               Tomás Buratovich - Final Física Computacional    02/2023                     !
!--------------------------------------------------------------------------------------------!
 


PROGRAM main_double_pendulum
USE solver

IMPLICIT NONE 

! Parameters 
INTEGER, PARAMETER :: n=4

! External subroutines
EXTERNAL derivatives

!-------------------------------------------------------------------------------!
!                             VARIABLE DICTIONARY                               !
!-------------------------------------------------------------------------------!
! y(n)     : Vector containing the info of w(omega) = d(theta)/dt               !
! dy_dx    : Vector containing the first derivative of w and seconf of theta    !
! y_out(n) : Vector that stores the solutions given by the RK4 method           !
! g        : Gravity                                                            !
! l1, l2   : Longitude of each individual pendulum                              !
! m1, l2   : Masses of each individual pendulum                                 !
! err      : IOSTAT variable nickname (displays error message when IOSTAT !=0)  !
! i        : Iteration variable                                                 !
! x0, xf   : initial and final values of Independent variables (in this case    !
!            as it is a physical system represents the initial and final time)  !
! xx1, yy1 : Cartesian coordinates of pendulum 1                                !
! xx2, yy2 : Cartesian coordinates of pendulum 2                                !
! n_steps  : Stores the time step used for discretization                       !
! nn       : Vector dimension variable passed to subroutine                     !
!-------------------------------------------------------------------------------!

! Variable definition 
REAL(8) :: y(n), dy_dx(n), y_out(n)
REAL(8) :: pi
REAL(8) :: g,l1,l2,m1,m2,m 
REAL(8) :: h
INTEGER :: nn, err, i, n_steps, ii
REAL(8) :: x0,xf,x
REAL(8) :: xx1,yy1,xx2,yy2

! Define pi value
pi = ACOS(-1.0d0)  
nn = n

! Open file to read the parameters of the system 
OPEN(UNIT=12,FILE='parameters.inp',STATUS='old', IOSTAT=err)
IF (err .ne. 0) STOP 'parameters.inp is missing'

READ(12,*)
READ(12,*) g
READ(12,*) l1
READ(12,*) l2
READ(12,*) m1
READ(12,*) m2
CLOSE(12) 
m=m1+m2

! Open file to read the initial conditions of the system
OPEN(UNIT=13,FILE='initial_conditions.inp',STATUS='old', IOSTAT=err)
IF (err .ne. 0) STOP 'initial_conditions.inp is missing'

READ(13,*)
READ(13,*) y(1)  ! phi1_0
READ(13,*) y(2)  ! w1_0
READ(13,*) y(3)  ! phi2_0
READ(13,*) y(4)  ! w2_0
READ(13,*) x0
READ(13,*) xf
READ(13,*) n_steps
CLOSE(13) 

! Compute initial position in cartesian coordinates
xx1 = l1*SIN(y(1))
yy1 = -l1*COS(y(1))
xx2 = xx1 + l2*SIN(y(3))
yy2 = yy1 - l2*COS(y(3))

! Compute step
h = (xf-x0)/dble(n_steps)

! Initialize x variable value to initial condition
x = x0

! Prepare the files to store the solution of the system
OPEN(UNIT=14,FILE='DP_polar_coordinates.dat',STATUS='new',IOSTAT=err)
IF (err .ne. 0) STOP 'DP_polar_coordinates already exists'
WRITE(14,*) '#t', 'phi1', 'w1', 'phi2', 'w2'
WRITE(14,*) x,y(1),y(2),y(3),y(4)

OPEN(UNIT=15,FILE='DP_xy_coordinates.dat',STATUS='new',IOSTAT=err)
IF (err .ne. 0) STOP 'DP_xy_coordinates.dat already exists'
WRITE(15,*) '#t', 'x1', 'y1', 'x2', 'y2'
WRITE(15,*) x,xx1,yy1,xx2,yy2

OPEN(UNIT=16,FILE='DP_xy_coordinates_util.dat',STATUS='new',IOSTAT=err)
IF (err .ne. 0) STOP 'DP_xy_coordinates_util.dat already exists'
WRITE(16,*) '#t', 'x1', 'y1', 'x2', 'y2'
WRITE(16,*) x,xx1,yy1,xx2,yy2


DO i=1,n_steps

  CALL derivatives(nn,g,l1,l2,m1,m2,m,x,y,dy_dx)
  CALL rk4(g,l1,l2,m1,m2,m,y,dy_dx,n,x,h,y_out,derivatives)

  x = x0 + dble(i)*h
  xx1 = l1*SIN(y(1))
  yy1 = -l1*COS(y(1))
  xx2 = xx1 + l2*SIN(y(3))
  yy2 = yy1 - l2*COS(y(3))

  ! Redifine y values tu use the subroutines again
  DO ii=1,nn
    y(ii) = y_out(ii)
  END DO

  ! Write the results in data files
  WRITE(14,*) x, y(1), y(2), y(3), y(4)
  WRITE(15,*) x, xx1, yy1, xx2, yy2

  IF (MOD(i,100) == 0) THEN
    WRITE(16,*) x, xx1, yy1, xx2, yy2
  END IF 

END DO

CLOSE(14)
CLOSE(15)
CLOSE(16)

PRINT*, '####### END OF PROGRAM #######'

STOP
END PROGRAM



! Displays the expressions of the derivatives given by the theory

SUBROUTINE derivatives(N,g,r1,r2,m1,m2,m,x,y,dy)
IMPLICIT NONE 

! Variable definition
INTEGER, INTENT(IN) :: N
REAL(8), INTENT(IN) :: g,r1,r2,m1,m2,m
REAL(8) :: x, y(N), dy(N)

dy(1) = y(2)

dy(2)=-(m2*r2*cos(y(1)-y(3))/(m*r1))*dy(4)-(m2*r2*sin(y(1)-y(3))/&
(m1*r1))*y(4)**2-g*sin(y(1))/r1

dy(3) = y(4)

dy(4)=-(r1*cos(y(1)-y(3))/r2)*dy(2)+(r1*sin(y(1)-y(3))/r2)*y(2)**&
2-g*sin(y(3))/r2

RETURN
END SUBROUTINE