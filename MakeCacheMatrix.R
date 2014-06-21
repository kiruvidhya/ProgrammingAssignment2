makeCacheMatrix <- function(x = matrix()) {
  # initializing inverse matrix with NULL
  
  s <- NULL
  
  # setting the matrix to an object  
  set <- function(y) {
    x <<- matrix(y)
    s <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse matrix 
  setinv <- function(solve) s <<- solve
  
  # Get the inverse matrix
  getinv <- function() s
  
  # List all the functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x=matrix(), ...) {
# Get the inverse matrix if it is cached  
 s <- x$getinv()
  if(!is.null(s)) {
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