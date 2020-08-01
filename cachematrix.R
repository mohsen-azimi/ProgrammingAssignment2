## This function creates a matrix and caches its inverse

makeCacheMatrix <- function( mat = matrix() ) {
  
  # Initialize  
  inv <- NULL
  
  # Set the matrix
  set <- function( matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    mat
  }
  
  # Set the inverse 
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse
  getInverse <- function() {
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Calculates  the inverse of the matrix above

cacheSolve <- function(x, ...) {
  
  # Inverse of 'x'
  mat <- x$getInverse()
  
  # Inverse if its already set
  if( !is.null(mat) ) {
    message("getting cached data...")
    return(mat)
  }
  
  # Get the matrix 
  data <- x$get()
  
  # Calculate the inverse 
  mat <- solve(data) %*% data
  
  # Set the inverse 
  x$setInverse(mat)
  
  # Return the matrix
  mat
}

