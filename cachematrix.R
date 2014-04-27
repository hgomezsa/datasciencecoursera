# Caches a Matrix and calculate its inverse
 

# Set of functions to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #Variable that will store the matrix inverse
  matrixinverse <- NULL
  #Set function (setter)
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  #Get function (getter)
  get <- function() x
  #Setinverse function (setter)
  setinverse <- function(inverse) matrixinverse <<- inverse
  #Getinverse function (getter)
  getinverse <- function() matrixinverse
  #List to be returned with the four functions. Each function will return a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# Calculates the matrix inverse taking the vector-based functions as example

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getinverse()
  # If inverse is already calculated, return the cached one
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  # Get the matrix and calculate the inverse
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
