## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){             ## set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ## get the value of the matrix
        setinverse <- function(inverse) inv <<- inverse ## set the value of the inverse
        getinverse <- function() inv    ## get the value of the inverse
        list(ser = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){  ## first checks to see if the mean has already been calculated
                message("getting catching data")
                return(inv)  ## skips the computation
        }
        data <- x$get()
        inv <- solve(data,...)  ## calculates the inverse
        x$setinverse(inv)         ## sets the value of the inverse
        inv
}
