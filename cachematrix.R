## Cache and retrive of matrix inverse functions Made by ksetdekov for
## Programming Assignment 2: Lexical Scoping in R Programming course by Johns
## Hopkins University

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve will retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


## Code for testing
w<-matrix(sample(1:25,replace = FALSE),nrow = 5, ncol = 5)
y<-makeCacheMatrix(w)
print("first calulation of inverse - not cached")
print(cacheSolve(y))
print("second calulation of inverse - already cached")
w_inv<-cacheSolve(y)
print(w_inv)

## multiplication to test if results of cacheSolve is an inverse matix
wres<-w %*% w_inv 
## test for inversion being true
if (dim(wres)[1]==round(sum(wres),digits = 6)) {
        print("Inversion is correct and W*W-1 equals unity matrix")
}else{
        print("Inversion seems not correct and W*W-1 does not equal unity matrix")
}