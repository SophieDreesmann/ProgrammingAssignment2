## This function creates a special "matrix" object that can cache its inverse
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the Inverse
## 4. get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function () inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve retrieves the inverse from the cache.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Computes inverse, if not yet computed
  mat <- x$get()
  inv <- solve(mat, ...)
  
  ## Cache the inverse
  x$setinverse(inv)
  
  ## Return the inverse
  inv
}