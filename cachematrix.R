#The following two functions are used to cache the inverse of a matrix

# makeCacheMatrix creates a list of functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inver_ <- NULL
    set <- function(y) {
        x <<- y
        inver_ <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse_) inver_ <<- inverse_
    get_inverse <- function() inver_
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# Return a matrix that is the inverse of 'x'
# The following function returns the inverse of the matrix. 
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inver_ <- x$get_inverse()
    if(!is.null(inver_)) {
        message("Here we take data from cache")
        return(inver_)
    }
    our_matrix <- x$get()
    inver_ <- solve(our_matrix)
    x$set_inverse(inver_)
    return(inver_)
}
