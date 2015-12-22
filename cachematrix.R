## The aim of the function set is to be able to cache the inverse of matrices, and allow it to
## looked up rather than recalculating it every time.  

## makeCacheMatrix takes a matrix 'x' and creates a reference list that sets the values of 'x' and
## its inverse into the cache, and retrieves the values of 'x' and its inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
inversevalue <- NULL
set <- function(y) {
                    x <<- y
                    inversevalue <<- NULL
                    }
get <- function() x
setinv <- function(inv) inversevalue <<- inv  
getinv <- function() inversevalue

list(set = set, get = get, setinv=setinv, getinv=getinv)

}


## cacheSolve  checks whether the  inverse of 'x' is already in the cache.  
## If so, it pulls the inverse from the cache and returns the value.  If not, it calculates 
## the inveerse and stores it in the cache.

cacheSolve <- function(x, ...) {
        
          ## # Checks if the inverse it is stored in cache, and if so retrieve it from cache and return it.
          inversevalue <- x$getinv()
            
          if(!is.null(inversevalue)) {
                          message("getting cached data")
                          return(inversevalue)
                          }
  
  # if not, calculate the inverse and store it in the cache.             
  data <- x$get()
  inversevalue <- solve(data, ...)
  x$setinv(inversevalue)
  inversevalue
  
  
  
}
