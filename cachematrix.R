## Two functions, one which creates a matrix which can cache its inverse,
## and one which computes the inverse of the first function. In the case that
## it has already been solved, it pulls the cached inverse instead.

## Creates a cacheable matrix which can be input to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Checks for a proper matrix
  if(!is.matrix(x)){
    stop("Input is not a proper matrix.") 
  }
  
  Xinverse <- NULL
  
  set <- function(y){
    x <<- y
    Xinverse <<- NULL
  }
  
  ##Functions for getting and setting the inverse value of the matrix
  get <- function() x
  setinverse <- function(solve) Xinverse <<- solve
  getinverse <- function(solve) Xinverse
  
  ##Creates an output list to store data
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the matrix made with the above function.
## If the inverse has already been computed, simply returns it.

cacheSolve <- function(CacheX, ...) {
    ## Checks if the inverse has been cached
    Xinverse <- CacheX$getinverse()
    if(!is.null(Xinverse)){
      message("Acquiring data...")
      return(Xinverse)
    }
    
    ##Creates the inverted data if it was not already cached
    work <- CacheX$get()
    Xinverse <- solve(work)
    CacheX$setinverse(Xinverse)
    Xinverse
}
