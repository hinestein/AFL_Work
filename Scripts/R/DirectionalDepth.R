#Made new mahalanobis depth function with lower tolerance. Tolerance can be lowered if need be
mahalanobis_1 = function(x, center, cov, inverted = FALSE, tolerance = 10^(-300)){
  x <- if (is.vector(x)) 
    matrix(x, ncol = length(x))
  else as.matrix(x)
  if (!isFALSE(center)) 
    x <- sweep(x, 2L, center)
  if (!inverted) 
    cov <- solve(cov, tol = tolerance)
  setNames(rowSums(x %*% cov * x), rownames(x))
}

#Change mahalanobis depths to mahalanobis_1get
Directional.Depth = function(X.list, method = "Mahalanobis", scale = FALSE){
  d = length(X.list)
  if(!inherits(X.list,"list")){
    stop("X.list must be in the form of a list")
  }
  nr = NULL
  for(i in 1:d){
    nr = c(nr, nrow(X.list[[i]]))
  }
  if(length(unique(nr)) > 1){
    stop("Dimensions are not equal")
  }
  nr = nr[1]
  nc = NULL
  for(i in 1:d){
    nc = c(nc, ncol(X.list[[i]]))
  }
  nc = nc[1]
  if(length(unique(nc)) > 1){
    stop("Dimension are not equal")
  }
  if(scale){
    for(i in 1:d){
      X.list[[i]] = scale(X.list[[i]])
    }
  }
  if(method == "Mahalanobis"){
    depth3 = NULL
    for(i in 1:nc){
      my_sample <- NULL
      for(j in 1:d){
        my_sample = cbind(my_sample, X.list[[j]][,i])
      }
      mu <- apply(my_sample,2,mean)
      sigma <- cov(my_sample)
      m_dist <- mahalanobis_1(my_sample, mu, sigma)
      m_depth <- 1/(1 + m_dist)
      depth3 = cbind(depth3, m_depth)
    }
  }else{
    depth3 = matrix(0, nrow = nr, ncol = nc)
    for(i in 1:nc){
      my_sample <- NULL
      for(j in 1:d){
        my_sample = cbind(my_sample, X.list[[j]][,i])
      }
      for(j in 1:nr){
        depth3[j,i] = depth(my_sample[j,], my_sample, method = method)
      }
    }
  }
  depth.dash = matrix(0, nrow = nr, ncol = nc)
  for(i in 1:nc){
    x1 <- NULL
    for(j in 1:d){
      x1 = cbind(x1, X.list[[j]][,i])
    }
    zt = which(depth3[,i] == max(depth3[,i]))[1]
    sigma <- cov(x1)
    EVV=eigen(sigma)
    vec=EVV$vectors
    for(j in 1:nrow(depth3)){
      z1 = x1[zt,d]
      if(d > 1){
        for(k in 1:(d-1)){
          z1 = z1 - (vec[k,1]/vec[d,1]) * (x1[j,k] - x1[zt,k])
        }
      }else{
        z1 = z1
      }
      if(x1[j,d] < z1){
        depth.dash[j,i] = depth3[j,i]
      }else{
        depth.dash[j,i] = 2 * depth3[zt,i] - depth3[j,i]
      }
    }
  }
  depth.dash
}