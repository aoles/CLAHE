# The following functions are based on the code from `grDevices::convertColor`,
# but tailored to the particular use case to improve performance

rgb2lab = function(x) {
  c2to3 <- grDevices:::c2to3
  red <- c(0.6400, 0.3300)
  green <- c(0.3000, 0.6000)
  blue <- c(0.1500, 0.0600)
  white <- whitexyz <- c2to3(c(x = 0.3137, y = 0.3291))
  rgb <- rbind(c2to3(red),
               c2to3(green),
               c2to3(blue))
  S <- drop(whitexyz %*% solve(rgb))
  M <- S * rgb
  
  # first convert to XYZ
  x = ifelse(x < 0.04045, x/12.92, ((x+0.055)/1.055)^2.4) #dogamma
  XYZ = x %*% M
  
  # then convert to LAB
  epsilon <- 216/24389
  kappa <- 24389/27
  
  xyzr <- t(XYZ)/white
  fxyz <- ifelse(xyzr <= epsilon, (kappa*xyzr+16)/116, xyzr^(1/3))
  
  lab = cbind(L = 116*fxyz[2L,]-16,
              a.x = 500*(fxyz[1L,]-fxyz[2L,]),
              b = 200*(fxyz[2L,]-fxyz[3L,]))
  
  lab*0.01
}

lab2rgb = function(x) {
  # RGB constants
  c2to3 <- grDevices:::c2to3
  red <- c(0.6400, 0.3300)
  green <- c(0.3000, 0.6000)
  blue <- c(0.1500, 0.0600)
  white <- whitexyz <- c2to3(c(x = 0.3137, y = 0.3291))
  rgb <- rbind(c2to3(red),
               c2to3(green),
               c2to3(blue))
  S <- drop(whitexyz %*% solve(rgb))
  M <- S * rgb
  
  # first convert to XYZ
  Lab = x / 0.01
  
  epsilon <- 216/24389
  kappa <- 24389/27
  
  yr <- ifelse(Lab[,1L] < kappa*epsilon, Lab[,1L]/kappa, ((Lab[,1L]+16)/116)^3)
  fy <- ifelse(yr <= epsilon, (kappa*yr+16)/116, (Lab[,1L]+16)/116)
  fx <- Lab[,2L]/500+fy
  fz <- fy-Lab[,3L]/200
  
  zr <- ifelse(fz^3 <= epsilon, (116*fz-16)/kappa, fz^3)
  xr <- ifelse(fx^3 <= epsilon, (116*fx-16)/kappa, fx^3)
  
  XYZ = rbind(X = xr, Y = yr, Z = zr) * white
  XYZ = t(XYZ)
  
  # convert from XYZ to RGB
  x = XYZ %*% solve(M)
  rgb = ifelse(x <= 0.0031308, 12.92*x, 1.055*x ^ (1/2.4)-0.055) # ungamma
  
  # trim and clip
  rgb <- round(rgb,5)
  rgb[rgb < 0] <- 0
  rgb[rgb > 1] <- 1
  
  rgb
}
