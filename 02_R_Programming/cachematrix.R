##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  # Initialize the inverse of the matrix "mat"
  invMat <- NULL
  
  ## Setter method of the matrix "mat"
  set <- function(matrix) {
    mat <<- matrix
    invMat <<- NULL
  }
  
  ## Getter method of the matrix "mat"
  get <- function() {
    mat
  }

  ## Setter method of the inverse of the matrix "mat", i.e. "invMat"
  setInverse <- function(inverse) {
    invMat <<- inverse
  }
  
  ## Method to get the inverse of the matrix "mat", i.e."invMat"
  getInverse <- function() {
    ## Return the inverse property
    invMat
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- mat$getInverse()
  
  ## Just return the inverse if its already set
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  
  ## Calculate the inverse using matrix multiplication
  invMat <- solve(mat$get())
  
  ## Set the inverse to the object
  mat$setInverse(invMat)
  
  ## Return the matrix
  invMat
}