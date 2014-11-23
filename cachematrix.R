## This function creates special "matrix",
## which is really a list of four functions:
## set, get, set.inv, get.inv
## These four functions are defined inside makeCacheMatrix function
## so makeCacheMatrix function is the environment of these functions.
## If one is similar with object programming
## it is convinient to treat function makeCacheMatrix
## as a Constructor of some class. 
## This class has two private attributes: x and s
## and 4 public methods to set and get values of these attributes
## It is also worth to remember that whenever x is assigned, variable s is set to NULL

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    set.inv <- function(inv) s <<- inv
    get.inv <- function() s
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}


## This function take as an argument a special "matrix"
## constructed with the previous function
## It first checks if varriable s is different from NULL
## If true, it simply returns s
## If false, it gets value of x (which is stored in makeCacheMatrix), inverses it,
## assignes the result to s in makeCacheMatrix (function set.inv)
## and lastly it returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$get.inv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$set.inv(s)
    s
}


