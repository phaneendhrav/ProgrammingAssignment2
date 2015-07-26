## makeCascheMatrix creates a list with following 4 functions
## 1. set     - Used to set the value of the matrix
## 2. get     - Used to get the matrix
## 3. getinv  - Used to get the inverse of the matrix 
## 4. setinv  - Used to set the inverse of the matrix in list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is used set and get the inverse of the matrix to a list

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  #Check for the existance of the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Compute the inverse if cache does not exist
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  inv
}