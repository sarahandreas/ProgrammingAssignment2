##
## Programming-Assignment2
##

## Functions in order to compute the inverse of a matrix or cache the inverse if it had already been computed (instead of recomputing it)


##
## function: makeCacheMatrix
##
## makeCacheMatrix creates a special "matrix" object which can be used in the function cacheSolve in oder to cache the inverse
##
## e.g. by
## myMatrix <-  makeCacheMatrix()
## or by
## myMatrix <-  makeCacheMatrix( matrix(c(1,1,1,2,3,3,3,4,5),3,3) )


makeCacheMatrix <- function(m = matrix()) {
    
    # initialize the inverse inv of the matrix m as NULL (placeholder)
    inv <- NULL
    
    # if the matrix m is changed by the user to a new matrix mat ( e.g. by myMatrix$set(matrix(c(2,1,3,2),2,2)) )
    # then set the matrix m to the new matrix mat
    # and re-initialize the inverse matrix inv as NULL
    set <- function(mat) {
        m <<- mat
        inv <<- NULL
    }
    
    # retreive the matrix currently stored in m 
    # ( e.g. by myMatrix$get() )
    get <- function() m

    # set the inverse matrix inv to be equal to inverse
    # ( e.g. by m$setinverse(myinv) as done in the next function cacheSolve )
    setinverse <- function(inverse) inv <<- inverse

    # retreive the current value of the inverse matrix inv ( either equal to NULL or equal to the inverse matrrix of m) 
    # (e.g. by myMatrix$getinverse() )
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


##
## function: cacheSolve
##
## cacheSolve takes a "matrix" object returned by makeCacheMatrix and 
##  - either computes the inverse
##  - or retreives the inverse from the cache if the inverse has already been computed and the matrix has not changed
##
## e.g. by
## cacheSolve(myMatrix)


cacheSolve <- function(m, ...) {
    
    # retreive the value currently stored as the inverse of m ( either NULL if the inverse has not 
    # been computed yet or the actually inverse matrix inv if it has already been computed previously )
    myinv <- m$getinverse()

    # check if the inverse matrix myinv is NULL (i.e. has not been computed yet) or different from NULL
    if(!is.null(myinv)) {

        # if the inverse matrix myinv is different from NULL, 
        # this means it has already been computed and can therefore be returned from the cached value
        message("getting inverse matrix from cached data")
        
        # then leave the function cacheSolve ( without having to re-compute the inverse matrix with solve() )
        return(myinv) 
    }
    
    
    # if the inverse matrix myinv is NULL, it has not been computed yet and therefore needs to be computed with solve()
    # initialize a new temporary internal matrix mymat as the current matrix stored as m
    mymat <- m$get()
    
    # compute the inverse matrix myinv of this matrix mymat with the solve()-function
    myinv <- solve(mymat, ...)
    
    # store this newly computed inverse matrix myinv as the inverse inv of the matrix m in the "matrix" object
    m$setinverse(myinv)
    
    # return this newly computed inverse matrix myinv
    return(myinv)
    
}
