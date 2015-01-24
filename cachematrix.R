# This methods allow you to cache inverse of given matrix and reuse it as long, as your matrix has not been change

# This will create special 'matrix' object with contains methods:
# set - to set new matrix
# get - to get matrix value, that was previously set
# setsolve - to set inverse of given matrix (solve(matrix))
# getsolve - to get inverse of given matrix, that was previously set
# 
# This function has one parameter - x, which is not required. x will be set as initial matrix by set method
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    # set new value and unset old inverce matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # get actual matrix
    get <- function() {
        x
    }
    # set new inversion
    setsolve <- function(solve) {
        s <<- solve
    }
    # get inversion
    getsolve <- function() {
        s
    }
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


# This method will compute inverse of special 'matrix' object created by function makeCacheMatrix
# If inverse was already calculate and data in our object wasn't changed, this will return previously calculated value
#
# First parameter of this function - x - is an object, that was created by makeCacheMatrix
# rest of parameters will be pass to solve method
cacheSolve <- function(x, ...) {        
    s <- x$getsolve()
    
    # return cached inverse, if we have it saved in x object
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # get actual matrix from x object
    data <- x$get()
    # compute inversion
    s <- solve(data, ...)
    # set inverse matrix in x object
    x$setsolve(s)
    s
}
