## These functions will provide a way of caching the result
## of a resource intense or long running function so that it's 
## result may be returned without without re-incurring the 
## impact of that function.

## This first function will return a list of functions to provide
## the ability to set or retrieve the original value, execute a 
## function against that value and cache the results of the function
## which can also be retrieve from that cache for subsequent executions.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() { 
        x 
    }
    setinverse <- function(solve) { 
        s <<- solve
    }
    getinverse <- function() { 
        s
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Using the function above, this next function allows us to retrieve a 
## cached result of the solve function from the previous execution of this 
## function. If this is the first execution of this function, if will 
## execute the solve function and then cache the results to be returned on 
## the next execution of this function.

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setinverse(s)
    s
}
