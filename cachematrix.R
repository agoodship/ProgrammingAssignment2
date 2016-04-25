## The functions below cache the value of a computation, allowing R to lookup
## a value in the cache and return that value rather than re-running the
## computation

## the makeCacheMatrix  function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set, 
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }



## cacheSolve computes the inverse of the matrix created by makeCacheMatrix
## If the inverse already exists in the cache, it is retrieved from the cache
## rather than re-doing the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                        inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                dat <- x$get()
                inv <- solve(dat, ...)
                x$setInverse(inv)
                print(inv)
        }
        

