## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ # Set function allows us to change the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x # Get function calls the matrix
        setinv <- function(solve,x,...) inv <<- solve # Stores the value of the inverted matrix
        getinv <- function() inv # Returns the value of the inverted matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv) # Stores all 4 functions
}

## cacheSolve function computes the inverse of the special matrix returned by
## makeCacheMatrix. if the inverse has been calculated and matrix is the same,
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() # We are verifying inv, stored previously via getinv, exists and is not null.
        if(!is.null(inv)) { 
                message("getting cached inverse")
                return(inv)
        }
        thematrix <- x$get() # If inv is null, calculate inverse of matrix x
        inv <- solve(thematrix)
        x$setinv(inv)
}