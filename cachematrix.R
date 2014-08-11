## Computing the inverse of a matrix can be costly, and caching it can be
## a worthwhile facility.  The following functions implement just that.
##
## Note that it is being assumed that the matrix will be invertible.

## Construct an object that holds the matrix and its precalculated inverse.
## The inverse may be NULL to indicate that it has not been created.
##  - setMatrix(m) sets the matrix to m (and clears the inverse)
##  - getMatrix() loads the matrix
##  - setInverse(i) sets the matrix inverse to m
##  - getInverse() loads the matrix inverse; the returned value may be NULL

makeCacheMatrix <- function(mat = matrix()) {
        imat <- NULL
        setMatrix <- function (m) {
                mat <<- m
                imat <<- NULL
        }
        getMatrix <- function () {
                mat
        }
        setInverse <- function (i) {
                imat <<- i
        }
        getInverse <- function () {
                imat
        }
}


## The following function wraps around solve() and works on inverse-cached matrices
## such as constructed by makeCacheMatrix() above.  When the inverse is already
## present in the cached object, it will be loaded instead of recalculated.
##
## Note: The reliable functioning of this operation revolves around the use of
##       setMatrix() to set the matrix in all other code, rather then accessing the
##       variables directly; there is no such thing as encapsulation.

cacheSolve <- function(m, ...) {
        i <- m$getInverse ()
        if (is.null (i)) {
                message "Recalculating inverse matrix"
                i <- solve (m$getMatrix (), ...)
                m$setInverse (i)
        }
        i
}
