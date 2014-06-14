## Cousera R Programming 
## Second Programing Assignment
## David Jones (DavidCJones)
##
## Example of caching values of time consuming operations in the enclosing environment
##    Note the assumption is that the matrix can be inversed.
##
## Functions 
##   makeCacheMatrix(<matrix>)  creates a special "matrix" object to cache its inverse in the parrent environment.


makeCacheMatrix <- function(x = matrix()) {
    globInv <- NULL
    set <- function(y) {
      x <<- y
      globInv <<- NULL
    }
    get <- function() x
    setInverse <- function(matInv) globInv <<- matInv
    getInverse <- function() globInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  



##   makeCacheSolve(<matrix>)   computes the inverse of the "matrix" returned by `makeCacheMatrix`. If the inverse has
##                              already been calculated (and the matrix has not changed), then the function will 
##                              retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    globInv <- x$getInverse()
    if(!is.null(globInv)) {
      message("getting cached data")
      return(globInv)
    }
    matW <- x$get()
    globInv <- solve(matW, ...)
    x$setInverse(globInv)
    globInv
  }

