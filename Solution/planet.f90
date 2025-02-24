PROGRAM planetary_orbits
  IMPLICIT NONE
  INTEGER, PARAMETER :: num_steps = 300
  REAL, PARAMETER :: pi = 3.14159265358979
  REAL, PARAMETER :: dt = 0.01  ! 時間刻み (年単位)
  INTEGER :: i
  REAL :: t, theta_sun, theta_venus, theta_earth, theta_mars
  REAL :: x_sun, y_sun, x_venus, y_venus, x_earth, y_earth, x_mars, y_mars
  REAL :: x_venus_rel, y_venus_rel, x_mars_rel, y_mars_rel
  REAL, PARAMETER :: r_sun = 0.0, T_sun = 1e-5
  REAL, PARAMETER :: r_venus = 1.082, T_venus = 87.97 / 365.0
  REAL, PARAMETER :: r_earth = 1.496, T_earth = 1.0
  REAL, PARAMETER :: r_mars = 2.279, T_mars = 1.88
  REAL :: omega_sun, omega_venus, omega_earth, omega_mars
  OPEN(10, FILE='planetary_orbits.dat', STATUS='REPLACE')

  ! 角速度の計算 (2π/T)
  omega_sun = 2.0 * pi / T_sun
  omega_venus = 2.0 * pi / T_venus
  omega_earth = 2.0 * pi / T_earth
  omega_mars = 2.0 * pi / T_mars

  DO i = 0, num_steps
    t = i * dt

    ! 各惑星の太陽中心の位置 (円運動)
    theta_sun = omega_sun * t
    theta_venus = omega_venus * t
    theta_earth = omega_earth * t
    theta_mars = omega_mars * t
    
    x_sun = r_sun * COS(theta_sun)
    y_sun = r_sun * SIN(theta_sun)
    
    x_venus = r_venus * COS(theta_venus)
    y_venus = r_venus * SIN(theta_venus)
    
    x_earth = r_earth * COS(theta_earth)
    y_earth = r_earth * SIN(theta_earth)
    
    x_mars = r_mars * COS(theta_mars)
    y_mars = r_mars * SIN(theta_mars)

    ! 地球基準の座標系へ変換
    x_venus_rel = x_venus - x_earth
    y_venus_rel = y_venus - y_earth
    
    x_mars_rel = x_mars - x_earth
    y_mars_rel = y_mars - y_earth

    ! ファイルに出力
    WRITE(10,*) t, x_venus_rel, y_venus_rel, x_mars_rel, y_mars_rel
  END DO
  
  CLOSE(10)

  PRINT *, 'Simulation complete. Data saved to planetary_orbits.dat'

END PROGRAM planetary_orbits
