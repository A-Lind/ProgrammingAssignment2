# makeCacheMatrix: creates a special "matrix" object (a list of functions) that can cache the inverse of a given matrix:
# The list of functions in makeCachematrix consists of: 
# 1.Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        # Set-function for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get-function for the matrix
        get <- function() x
        
        # Set-function for the inverse (including the caching <<- operator)
        setinv <- function(inverse) inv <<- inverse
        # Get-function for the inverse
        getinv <- function() inv
        
        # Return the special "matrix" object with the list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: is a function to compute the inverse of a matrix (returned by makeCacheMatrix above). But it checks whether or  
# not the inverse has already been computed and returns the cached inverse matrix if that is true
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        # If the inverse is already computed, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse is not already computed, do so
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse, so we don't have compute it the next time
        x$setinv(inv)
        
        # Return the inverse of the matrix
        inv
}
