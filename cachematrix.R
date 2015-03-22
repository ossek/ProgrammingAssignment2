## We want a structure that represents a matrix
##  and a 'cache' for its inverse.
##The first function is a factory function to create this structure,
##  The second is a function to attempt to use the cached inverse,
##  and if cache empty, compute inverse and cache the inverse.

## Factory function for the structure. 
## Requires that matrix x is invertible.
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    m_matrix  <- x

    getMatrix <- function()
    {
        m_matrix
    }

    getInverse <- function()
    {
        m_inverse
    }

    ## param 'inverse' should be a matrix, the inverse of m_matrix
    setInverse <- function(inverse)
    {
        m_inverse <<- inverse
    }

    list(getMatrix=getMatrix,
         getInverse=getInverse,
         setInverse=setInverse)

}


## Look for cached inverse in x and if not found, compute
## and cache it.  Assume x is a structure created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(is.null(inverse))
    {
        inverse <- solve(x$getMatrix(),...)
        x$setInverse(inverse)
    }
    inverse
}
