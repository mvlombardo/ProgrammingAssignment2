## These functions will enable caching a matrix and then solving the inverse of
## a matrix in case the inverse hasn't already been computed

## This function will enable the following 4 things to be done
## 1. Setting a matrix with the set() function
## 2. Getting a matrix with the get() function
## 3. Setting the inverse of the matrix
## 4. Getting the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
		
		# set i to NULL initially
	    i <- NULL

	    # make a function to set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }# end function set

        # function to grab a matrix that has already been set
        get <- function() x

        # function to set the inverse of the matrix
        setinv <- function(inv) i <<- inv

        # function to grab the cached inverse of the matrix
        getinv <- function() i

        # return this list as output from the function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}# end function makeCacheMatrix


## This function will compute the inverse of a matrix if its not already cached
cacheSolve <- function(x, ...) {
	    # set inv variable to the cached matrix inverse
	    inv <- x$getinv()

	    # if a cached inverse exists, return this as the output of the function
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }# end if(!is.null(inv))

        # if the cached inverse doesn't exist, compute it
        data <- x$get() # first grab the actual matrix
        inv <- solve(data, ...) # now compute its inverse
        x$setinv(inv) # cache the inverse just computed
        inv # return the matrix inverse as the output of the function
}# end function cacheSolve






