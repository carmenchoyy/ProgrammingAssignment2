## The functions caches the inverse of a matrix

## Function that creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
cacheInv <- NULL
set <- function(y) {
  x <<- y
  cacheInv <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) cacheInv <<- inverse
  getinverse <- function() cacheInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes inverse of matrix returned by makeCacheMatrix; if inverse is already calculated, retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheInv<-x$getinverse()
  if(!is.null(cacheInv)) {
    message("getting cached data")
    return(cacheInv)
  }
  data<-x$get()
  cacheInv<-solve(data,...)
  x$setinverse(cacheInv)
  cacheInv
}
