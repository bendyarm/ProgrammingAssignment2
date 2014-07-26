## Define a wrapper on matrix that makes it easy
## to cache and access the inverse matrix.
## 
## There are two functions,
##   makeCacheMatrix(m): m_w_c
##     -- takes a regular matrix and returns a "matrix with cache" list structure
##        For the accessors see below.
##   cacheSolve(m_w_c)
##     -- takes a "matrix with cache" and caches the inverse matrix.
##        Gets an error if the matrix does not have an inverse.


## makeCacheMatrix(m)
## Takes a matrix as an argument
## and creates and returns a list containing
## four accessor functions:
## set(): sets the matrix
## get(): retrieves the matrix
## setinverse(): sets the inverse of the matrix
%%    DO NOT CALL THIS other than from makeCacheMatrix(). 
##    Reason: this is a bad factoring, but it is required by the assigment.
##    This structure makes it possible for the inverse to be out-of-sync of the matrix.
##    It would be better if there were no setinverse() function,
##    but instead getinverse() would compute the inverse
##    it if it has not yet been computed, or
##    just return it if it had already been computed.
## getinverse(): gets the inverse of the matrix, which has been
##    cached by cacheSolve(x), or NULL if cacheSolve(x) was not called
##
## NOTE on how to use these accessors:
## if mwc is a "matrix with cache" structure, you can retrieve the underlying
## matrix by doing:  mwc$get()
## Similarly, you can set the underlying matrix to mat with:  mwc$set(mat)
## and get the inverse (or NULL if not yet cached) with:  mwc$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
    ## inv is the inverse matrix.
    inv <- NULL
    set <- function(y) {
             # We could reject y here if y is not a square matrix, 
             # but since there are other matrices y that cannot
	     # be inverted even when y is square, just let the
	     # error happen when you fetch the inverse with getinv().
             x <<- y
	     # clear out the inverse matrix when the primary matrix changes
             inv <<- NULL
	     }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve(m_w_cache)
## Takes a matrix with a cache that is implemented as a list of accessor functions
## and checks to see if the inverse has already been calculated.
## If so, it just gets the inverse.  Otherwise it computes the inverse matrix
## and stores it in the m_w_cache before returning it.
cacheSolve <- function(x) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
	return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}
