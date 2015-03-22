## The functions below will cache the inverse of a matrix
## If the cache contains a value then the cache value is returned
## otherwise the inverse if calculated

## The makeCacheMatrix will create a cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y){
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsol <- function(sol) sol <<- sol
  getsol <- function() sol
  list(set = set, get = get , setsol= setsol, getsol = getsol)
  
  list()
}


## The cache solve function will return the cache if not null thus
## saving computing resources by accessing memory instead of recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsol()
        if(!is.null(sol)){
          message("getting the solve from the cache !!!")
          return(sol)
        }
      data <- x$get()
      sol <- solve(data,...)
      x$setsol(sol)
      sol
}
