

## The makeCacheMatrix function creates a CacheMatrix object that is a list of
## 2 matrices, the first being the argument of the function and the second
## being the initially empty matrix that will hold the inverse of the 
## first matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        if (!is.matrix(x)) {
                print("Input argument is not a matrix");
                return;
        }
        
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
        }
        get <- function() x
        setInverse <- function(x) invX <<- x
        getInverse <- function() invX
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The cacheSolve function checks to see if the inverse of the matrix is
## already present and if it is returns the cached matrix. If it is not present
## then it computes the inverse, caches it and returns it.
## It accepts as argument a cacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        invX <- x$getInverse();
        if(!is.null(invX)) {
                message("getting cached data");
                return(invX);
        }
        ## If the second matrix is not present then a) compute the inverse
        ## b) set the second matrix to the inverse
        data <- x$get();
        invX <- solve(data, ...);
        x$setInverse(invX);
        invX
        
}
