require(ggplot2)

f11=function(x) {
  x/(1+x)
}
f12=function(x) {
  1/(1+x)
}
f21=function(x) {
  12*exp(x)*x
}
f22=function(x) {
  12*exp(x)
}
f31=function(x) {
  sqrt(x)*x
}
f32=function(x) {
  sqrt(x)
}

solve = function(h) {
  a = c(0.1, 0.4, 0.9, 1)
  p = c(4, 3, 4)
  q = c(1, 0, 1)
  
  n = (a[4] - a[1]) / h
  x = seq(a[1], a[4], by = h)
  n = round(n)
  
  Ja = 0.7
  
  K = M = Q = matrix(c(0), n+1, n+1)
  F = vector("numeric", n)
  
  for (i in 2:n) {
    if(x[i] > a[1] & x[i] < a[2]) {
      K[i, i+1] = K[i, i-1] = -p[1] / h
      M[i, i+1] = M[i, i-1] = (q[1]*h) / 6
      K[i,i] = (2*p[1]) / h
      M[i,i] = (2*q[1]*h) / 3
      
      F[i] = (1/h) * (integrate(f11, x[i-1], x[i])[[1]] - x[i-1] * integrate(f12, x[i-1], x[i])[[1]] -
                        integrate(f11, x[i], x[i+1])[[1]] + x[i+1] * integrate(f12, x[i], x[i+1])[[1]])
    }
    
    if((x[i] > a[2]) && (x[i] < a[3])) {
      K[i, i+1] = K[i, i-1] = -p[2] / h
      M[i, i+1] = M[i, i-1] = (q[2]*h) / 6
      K[i,i] = (2*p[2]) / h
      M[i,i] = (2*q[2]*h) / 3
      
      # Возможно здесь разрыв
      F[i] = (1/h) * (integrate(f21, x[i-1], x[i])[[1]] - x[i-1] * integrate(f22, x[i-1], x[i])[[1]] -
                        integrate(f21, x[i], x[i+1])[[1]] + x[i+1] * integrate(f22, x[i], x[i+1])[[1]])
    }
    
    if((x[i] > a[3]) && (x[i] < a[4])) {
      K[i, i+1] = K[i, i-1] = -p[3] / h
      M[i, i+1] = M[i, i-1] = (q[3]*h) / 6
      K[i,i] = (2*p[3]) / h
      M[i,i] = (2*q[3]*h) / 3
      
      F[i] = (1/h) * (integrate(f31, x[i-1], x[i])[[1]] - x[i-1] * integrate(f32, x[i-1], x[i])[[1]] -
                        integrate(f31, x[i], x[i+1])[[1]] + x[i+1] * integrate(f32, x[i], x[i+1])[[1]])
    }
    
    if(x[i] == a[2]) {
      K[i, i] = (p[1]+p[2]) / h
      M[i, i] = (q[1]+q[2]) * h / 3
      K[i,i+1] = -p[2] / h
      M[i,i+1] = (q[2]*h) / 6
      K[i,i-1] = -p[1] / h
      M[i,i-1] = (q[1]*h) / 6
      
      F[i] = (1/h) * (integrate(f11, x[i-1], x[i])[[1]] - x[i-1] * integrate(f12, x[i-1], x[i])[[1]] -
                        integrate(f21, x[i], x[i+1])[[1]] + x[i+1] * integrate(f22, x[i], x[i+1])[[1]])
      
    }
    
    if((x[i] == a[3])) {
      K[i, i] = (p[2]+p[3]) / h
      M[i, i] = (q[2]+q[3]) * h / 3
      K[i,i+1] = -p[3] / h
      M[i,i+1] = (q[3]*h) / 6
      K[i,i-1] = -p[2] / h
      M[i,i-1] = (q[2]*h) / 6
      
      F[i] = (1/h) * (integrate(f21, x[i-1], x[i])[[1]] - x[i-1] * integrate(f22, x[i-1], x[i])[[1]] -
                        integrate(f31, x[i], x[i+1])[[1]] + x[i+1] * integrate(f32, x[i], x[i+1])[[1]])
    }
  }
  
  K[1,1] = p[1] / h
  K[1,2] = -p[1] / h
  M[1,1] = q[1]*h / 3
  M[1,2] = q[1]*h / 6
  F[1] = (1/h) * (x[2] * integrate(f12, x[1], x[2])[[1]] - integrate(f11, x[1], x[2])[[1]]) + Ja

  Q = K + M
  Q[1,1] = Q[1,1] + Ja
  
  u = c()
  u[n+1] = 10.5
  
  alpha = vector("numeric", n)
  beta  = vector("numeric", n)
  
  Q[n+1, n] = 0
  Q[n+1, n+1] = 1
  F[n+1] = u[n+1]
  
  alpha[n+1] = -Q[n+1,n] / Q[n+1, n+1]
  beta[n+1] = F[n+1] / Q[n+1, n+1]

  for (i in n:2) {
    alpha[i] = -Q[i,i-1] / (Q[i,i+1] * alpha[i+1] + Q[i,i])
    beta[i] = (F[i] - Q[i,i+1] * beta[i+1]) / (Q[i, i+1] * alpha[i+1] + Q[i,i])
  }

  u[1] = (F[1] - Q[1, 2] * beta[2]) / (Q[1,2] * alpha[2] + Q[1,1])
  
  for (i in 2:n) {
    u[i] = alpha[i] * u[i-1] + beta[i]
  }
  
  plot(0, 0, col="white", xlim=c(0.1, 1.2), ylim = c(-10,11.5))
  grid(col=1)
  
  points(x, u, col=2,lwd=3)
  
  U = c()
  xh = c()
  
  for (n in 1:(length(u)-1)) {
    xh[n] = x[n] + (h/2) 
    U[n] = (u[n+1] - u[n]) / h
  }

  pu1= -p[1] * U[which(xh > a[1] & xh < a[2])]
  pu2= -p[2] * U[which(xh > a[2] & xh < a[3])]
  pu3= -p[3] * U[which(xh > a[3] & xh < a[4])]
  pu = c(pu1,pu2,pu3)
  
  for (i in 1:(length(x)-1)) {
    points(c(x[i], x[i+1]), c(pu[i], pu[i]), type="l", col=4, lwd=2, title(h))
  }
  
#### невязка 
  U1 =  Ja - p[1]*U[1]
  print("u'(a)")
  print(U1)
#### невязка 
  
  # df  = data.frame(x = x,
  #                  y = u
  # )
  # 
  # ggplot(NULL) + 
  #   geom_line(data = df, aes(x, y))+ xlim(0.1, 1.2) + ylim(-5,11.5)

}
solve(0.01)