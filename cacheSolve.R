# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x=matrix(), ...) {
# Get the inverse matrix if it is cached  
 s <- x$getinv()
 
 #  The following if condition checks inverse is calculated and matrix hasn't changed
  if(!is.null(s) && all.equal(diag(2),x$get()%*%s)) {
    message("getting cached data")
    return(s)
  }
  
  # get the original matrix
  data <- x$get()
 
  # Convert Inverse Matrix
  s <- solve(data, ...)
  
  # Set Inverse Matrix and set them into cache.
  x$setinv(s)
  s
}