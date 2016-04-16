## Coursera R Programming- Week 3 Assignment
## Prepared by Scott Twichel 17 April 2016

# To see this script work, try entering the following lines into the console
# after sourcing the script file:

# testmatrix<- makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(testmatrix)
# cacheSolve(testmatrix)

# The first line will create a square matrix, and store the result of the 
# makeCacheMatrix in the testmatrix variable. Next the testmatrix will be 
# processed by the cacheSolve function and print out the inverse to the 
# command line. Running the cacheSolve function again will cause the 
# "getting cached data" message to display followed by the inverse matrix


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
