## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  inv <- null
  set <- function(y){
    x <<- y 
    inv <<- null
  }
  get <- function() x 
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get, 
       setInv = setInv,
       getInv = getInv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}