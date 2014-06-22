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