## The following functions will cache the inverse of a given matrix, and allow 
## you to return the cached data later.


## Provide a matrix, the function will set the matrix within the environment,
## run the inverse function, and assign the matrix to be picked up by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set=set, get=get, 
       setinverse = setinverse, 
       getinverse=getinverse)
}


## The function will check whether getinverse is null - will skip the inverse
## calculation if there is a value assigned to it, and then provide the inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
