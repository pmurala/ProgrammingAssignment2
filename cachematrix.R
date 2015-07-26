
# makeCacheMatrix funvtion does the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  inversematrix  <<- inverse
  getinverse <- function()  inversematrix 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The following function calculates the inverse of the matrix.
# It first checks if the inverse of matrix is already calculated and cached,
# If calculated, it gets from the cache, If it is not already calculated it calculates and then caches it.

cacheSolve <- function(x, ...) {
  inversematrix  <- x$getinverse()
  if(!is.null( inversematrix)) {
    message("getting cached data.")
    return( inversematrix)
  }
  matrix<- x$get()
  inversematrix  <- solve(matrix)
  x$setinverse( inversematrix )
  inversematrix 
}
