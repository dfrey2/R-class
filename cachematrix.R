## The following two functions are designed to save time by retrieving an inverted matrix from the Cache.
## It can be time consuming to repeatedly compute the inverse of a matrix.
## So instead we store them in memory and retrieve them.

## The first function creates a list which does the following:
## a. set the value of the matrix 
## b. get the value of the matrix
## c. set the inverse of the matrix
## d. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 
  ## The function will first store the cached value.
  ## It is initially set to null, if nothing is cached.
  Memory <- NULL
  
  ## Next we create the store the matrix in the working environment.
  set <- function(y){
    x <<- y
    Memory <<- NULL
  }
  
  ## Then we "get" the value of the matrix.
  get <- function() x
  ## The matrix is then inverted, and stored in cached memory
  setmatrix <- function(inverse) Memory<<- inverse
  ## The inverted matrix is retrieved from the cache.
  getmatrix <- function() Memory
  ## The created functions are returned to the working environment
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The second function searches the cache for the inverse of the matrix.
## If it's not there, then the function computes the inverse.
## NOTE: We assume the matrix is a square and invertible matrix.

cacheSolve <- function(x=matrix(), ...) {
  ## R will try to find the inverse of the matrix if stored in cache.
  Memory <- x$getmatrix()
  
  ## If it exists in the memory, the function will return the inverted matrix.
  ## Otherwise, the matrix needs to be created.
  if(!is.null(Memory)) {
    message( "retrieving cached data" )
    
    ## Matrix will be printed to console
    return(Memory)
  }
  
  ## The following creates the matrix since it doesn't exist.
  matrix <- x&get()
  Memory <- solve(matrix)
  x$setmatrix(inv)
  return(Memory)
}
