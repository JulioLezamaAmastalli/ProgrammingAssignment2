## MakeCacheMatrix and cacheSolve functions were created to calculate 
## the inverse of a Matrix and cache it. So that when it is required 
## again and if the matrix has not changed, it does not have to be 
## recalculated.

## The MakeCacheMatrix function creates a vector that has 4 functions 
## associated with it: 1) defines a new matrix, 2) shows the matrix, 
## 3) calculates the inverse and 4) shows the inverse. If the inverse 
## has already been calculated, it is not recalculated.

makeCacheMatrix <- function(A = matrix()) {
        inverse <- NULL
        setMatrix <- function(B) {
                if (all(B != A)) {
                        A <<- B
                        inverse <<- NULL
                }
        }
        getMatrix <- function() A
        setInverse <- function() {
                if (is.null(inverse)) {
                        inverse <<- solve(A)
                }
        }
        getInverse <- function() inverse
        list(
                setM = setMatrix,
                getM = getMatrix,
                setI = setInverse,
                getI = getInverse
        )
}


## The cacheSolve function gets the inverse of an array either cached 
## previously or computes it.

cacheSolve <- function(x, ....) {
        if (!is.null(x$getI())) {
                message("getting cache inverse")
                inv <- x$getI()
        } else{
                x$setI()
                inv <- x$getI()
        }
        inv
}
