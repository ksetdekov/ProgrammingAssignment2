## Cache and retrive of matrix inverse functions Made by ksetdekov for
## Programming Assignment 2: Lexical Scoping in R Programming course by Johns
## Hopkins University

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve will retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


## Code for testing
w<-matrix(sample(1:25,replace = FALSE),nrow = 5, ncol = 5)
w_inv<-solve(w)
wres<-w %*% w_inv
## test for inversion being true
round(sum(wres),digits = 6)==dim(wres)[1]
