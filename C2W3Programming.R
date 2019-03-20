# Create a function of matrix with setter and getter methods and setter and getter methods for inversion.
makeCacheMatrix <- function(m = matrix()) {
  mInverse  <- NULL
  set <- function(y){
    m <<- y
    mInverse  <<- NULL
  }
  get <- function() m
  setInverse <- function(solveMatrix) mInverse <<- solveMatrix
  getInverse <- function() mInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function Cache Inverse. Uses the inverse getter method if the inserve of matrix is available. Otherwise,
# it uses the inverse setter method to calculate the inverse and save into mInverse in the matrix function.
cacheSolve <- function(m, ...) {
  mInverse <- m$getInverse()
  if(!is.null(mInverse)){
    message("getting cached inverse.")
    return(mInverse)
  }
  data <- m$get()
  mInverse <- solve(data)
  m$setInverse(mInverse)
  mInverse     
}
