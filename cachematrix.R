## Put comments here that give an overall description of what your
## functions do

## This function reads and stores original and inverted matrices

makeCacheMatrix <- function(x = matrix()) {
        m.inv <- NULL
        set <- function(y) {  # Needed if you want to reset matrix
                x <<- y       # Changes the variable outside this function, so <<-
                m.inv <<- NULL
        }
        get <- function() {x}
        set.inv <- function(mi) {m.inv <<- mi}  # 1st call of cacheSolve
        get.inv <- function() {m.inv}           # 2nd call of cacheSolve
        list(set=set, get=get, 
             set.inv=set.inv, get.inv=get.inv)
}


## If inverted matrix already exists and nothing has changed, function returns the
## inverted matrix from cache. Otherwise it computes inverse of original matrix, 
## writes it down to the cache and prints. 

cacheSolve <- function(x, ...) {
        m.inv <- x$get.inv()
        if (!is.null(m.inv)) {
                message("getting cached data")
                return(m.inv)
        }
        data <- x$get()
        m.inv <- solve(data)
        x$set.inv(m.inv)
        m.inv
}
