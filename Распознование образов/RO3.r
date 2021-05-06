library(ggplot2)

dot2 = function(x1, x2) {
  return(x1$x*x2$x+x1$y*x2$y)
}

dot3 = function(x1, x2) {
  return(x1[1]*x2[1]+x1[2]*x2[2]+x1[3]*x2[3])
}

norm = function(x1, x2) {
  return(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2))
}

exit = function(z, zz) {
  eps = 0.00001
  rv = FALSE 
  for (i in 1:nrow(z)) {
    if ((eps < abs(z$x[i] - zz$x[i])) || (eps < abs(z$y[i] - zz$y[i]))) {
      rv = TRUE
    }
  }
  return(rv)
}

solve = function(sx, sy) {
  x0 = c(-3.22, -2.33,	-3.34,	-2.45,	-3.53,	-3.21,	-3.55,
         -2.71,	-0.06,	-0.54,	-0.28,	-0.07,	-0.94,	-0.37,
         0.14,	-0.51,	2.84,	2.97,	2.01,	3.28,	2.50)
  y0 = c(3.52,	2.08,	2.45,	2.61,	3.68,	3.03,	2.53,	
         3.26, -0.11,	0.03,	-0.41,	0.28,	-0.47, -0.55,
         0.18,	0.28,	-2.70,	-2.19,	-2.99,	-2.87,	-3.83)
  x = data.frame(x = x0, y = y0)
  eps = 0.00001
  h = 0.1
  K = 3
  idx = sample(1:nrow(x), K, replace = FALSE)
  print(idx)
  zx = c(x0[idx[1]], x0[idx[2]], x0[idx[3]])
  zy = c(y0[idx[1]], y0[idx[2]], y0[idx[3]])
  z = data.frame(x = zx, y = zy)
  print(z)
  zzx = c(x0[1], x0[1], x0[1])
  zzy = c(y0[1], y0[1], y0[1])
  zz = data.frame(x = zzx, y = zzy)
  w = list()
  while (exit(z, zz)) {
    zz = z
    for (i in 1:K) {
      df = data.frame(x = double(), y = double())
      w[[i]] = df
    }
    for (i in 1:nrow(x)) {
      D = seq(length.out = K)
      for (j in 1:K) {
        D[j] = norm(z[j,], x[i,])
      }    
      minD = D[1]
      minIdx = 1
      for (j in 2:K) {
        if (D[j] < minD) {
          minD = D[j]
          minIdx = j
        }
      }
      w[[minIdx]] = rbind(w[[minIdx]], x[i,])
    }
    for (i in 1:K) {
      z$x[i] = mean(w[[i]]$x)
      z$y[i] = mean(w[[i]]$y)
    } 
  }
  print(w[[1]])
  print(w[[2]])
  print(w[[3]])
  g = ggplot() +
    geom_point(data = z, aes(x = x, y = y), color = "green", size = 3) +
    xlim(-8, 8) + ylim(-8, 8)
  for (i in 1:K) {
    g = g + geom_point(data = w[[i]], aes(x = x, y = y), color = i + 3)
  }
  
  b = c(-4, 4)
  d1 = matrix(data = 0, nrow = nrow(z), ncol = nrow(z))
  for (j in 2:nrow(z)) {
    for (i in 1:(j-1)) {
      d1[i, j] = (-(z$x[i]-z$x[j])*b[1] + 0.5*dot2(z[i,], z[i,]) - 0.5*dot2(z[j,], z[j,])) /
        (z$y[i] - z$y[j])
    }
  }
  d2 = matrix(data = 0, nrow = nrow(z), ncol = nrow(z))
  for (j in 2:nrow(z)) {
    for (i in 1:(j-1)) {
      d2[i, j] = (-(z$x[i]-z$x[j])*b[2] + 0.5*dot2(z[i,], z[i,]) - 0.5*dot2(z[j,], z[j,])) /
        (z$y[i] - z$y[j])
    }
  }
  
  w = matrix(data = 0, nrow = nrow(z), ncol = 3)
  for (i in 1:nrow(z)) {
    w[i,] = c(z$x[i], z$y[i], -0.5*dot2(z[i,], z[i,]))
  }
  
  s = data.frame(x = sx, y = sy)
  p = c(sx, sy, 1)
  d = seq(length.out = nrow(w))
  for (j in 1:nrow(w)) {
    d[j] = dot3(p, w[j,])
  }
  maxD = d[1]
  maxIdx = 1
  for (j in 2:length(d)) {
    if (d[j] > maxD) {
      maxD = d[j]
      maxIdx = j
    }
  }
  g = g + geom_point(data = s, aes(x = x, y = y), fill = maxIdx + 3, shape = 24, size = 3)
  
  for (j in 2:nrow(z)) {
    for (i in 1:(j-1)) {
      b = c(-4, 4)
      p = c(b[1], d1[i, j], 1)
      for (k in 1:nrow(w)) {
        d[k] = dot3(p, w[k,])
      }
      maxD = d[1]
      maxIdx = 1
      for (k in 2:length(d)) {
        if (d[k] > maxD) {
          maxD = d[k]
          maxIdx = k
        }
      }
      viewIdx1 = 0
      for (k in 1:length(d)) {
        if (k != maxIdx) {
          if (abs(maxD - d[k]) < eps) {
            viewIdx1 = 1
          } 
        }
      }
      p = c(b[2], d2[i, j], 1)
      for (k in 1:nrow(w)) {
        d[k] = dot3(p, w[k,])
      }
      maxD = d[1]
      maxIdx = 1
      for (k in 2:length(d)) {
        if (d[k] > maxD) {
          maxD = d[k]
          maxIdx = k
        }
      }
      viewIdx2 = 0
      for (k in 1:length(d)) {
        if (k != maxIdx) {
          if (abs(maxD - d[k]) < eps) {
            viewIdx2 = 1
          } 
        }
      }
      viewIdx = (viewIdx1 + viewIdx2)
      viewIdx3 = viewIdx1
      nextPoint = b[1] + h
      if (viewIdx == 1) {
        d3 = 0
        while ((nextPoint < b[2]) && (viewIdx3 == viewIdx1)) {
          d3 = (-(z$x[i]-z$x[j])*nextPoint + 0.5*dot2(z[i,], z[i,]) - 0.5*dot2(z[j,], z[j,])) /
            (z$y[i] - z$y[j])
          p = c(nextPoint, d3, 1)
          for (k in 1:nrow(w)) {
            d[k] = dot3(p, w[k,])
          }
          maxD = d[1]
          maxIdx = 1
          for (k in 2:length(d)) {
            if (d[k] > maxD) {
              maxD = d[k]
              maxIdx = k
            }
          }
          viewIdx3 = 0
          for (k in 1:length(d)) {
            if (k != maxIdx) {
              if (abs(maxD - d[k]) < eps) {
                viewIdx3 = 1
              } 
            }
          }
          nextPoint = nextPoint + h
        }
        if (viewIdx1 == 0) {
          b[1] = nextPoint
          d1[i, j] = d3
        }
        else if (viewIdx2 == 0) {
          b[2] = nextPoint
          d2[i, j] = d3
        }
      }
      else if (viewIdx == 2) {
        viewIdx = 1
      }
      ld = c(d1[i, j], d2[i, j]) 
      df = data.frame(x = b, y = ld)
      g = g + geom_line(data = df, aes(x = x, y = y), color = "green", alpha = viewIdx)
    }
  }
  
  print(g)
}