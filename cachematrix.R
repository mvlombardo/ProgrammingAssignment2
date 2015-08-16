## These functions will enable caching a matrix and then solving the inverse of
## a matrix in case the inverse hasn't already been computed

## This function will enable the following 4 things to be done
## 1. Setting a matrix with the set() function
## 2. Getting a matrix with the get() function
## 3. Setting the inverse of the matrix
## 4. Getting the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }# end function set
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}# end function makeCacheMatrix


## This function will compute the inverse of a matrix if its not already cached
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }# end if(!is.null(inv))
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}# end function cacheSolve






