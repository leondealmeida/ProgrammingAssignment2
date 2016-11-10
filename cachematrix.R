## The following functions support caching the calculation of an inverted
## matrix. If the 'cacheable matrix' object is not modified, the functions
## will return the cached results rather than calculating the inverted matrix
## again.
## The code expects an invertible matrix to be passed for calculation.

## makeCacheMatrix creates a matrix object with specially defined methods
## that will store the result of the matrix inversion within its environment.
makeCacheMatrix <- function(x = matrix()) {
    inverse_cache <- NULL
    set <- function(y) {
        x <<- y
        inverse_cache <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse_cache <<- inverse
    get_inverse <- function () inverse_cache

    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse
    )
}

## cacheSolve uses the methods made available by makeCacheMatrix to leverage
## the caching of the calculation of the inverted matrix. The code provides
## extra output notifying the user when the cache is used.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("Using cached inverted matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse(inverse)
    inverse
}

## Testing
## [ Invertible matrix example sourced from
##   http://www.sosmath.com/matrix/inverse/inverse.html ]
# m <- rbind(c(1, 3, 2), c(-1, 0, 2), c(3, 1, -1))
# cacheable <- makeCacheMatrix(m)
# inverse = cacheSolve(cacheable)  # expect no 'cached' message here
# inverse = cacheSolve(cacheable)  # 'Using cached inverted matrix' message is
#                                  # displayed
# cacheable$set(m)                 # re-sets the base matrix, and clears the
#                                  # cache
# inverse = cacheSolve(cacheable)  # expect no 'cached' message here since the
#                                  # cache was cleared
# inverse = cacheSolve(cacheable)  # expect 'Using cached inverted matrix'
#                                  # message