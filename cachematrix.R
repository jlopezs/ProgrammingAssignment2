################
################
## Coursera R programming assignment 2 
## JLS, Dec 2014

################
################
## makeCacheMatrix: function to create a "special matrix": an object with
## an environment and functions to set and get the value of a matrix and
## its inverse (the latter is NULL if not available).
##
## Argument is a matrix, e.g. inputMatrix<-matrix(c(2,0,0,2),nrow=2,ncol=2)
##
## To set the data inside a "special matrix" named "sMatrix" use
##     sMatrix$set(inputMatrix)           : store a matrix
##     sMatrix$setinv(solve(inputMatrix)) : store the inverse of the matrix
##
## To get the data inside sMatrix use
##     sMatrix$get()        : returns the original matrix
##     sMatrix$getinverse() : returns the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

       ## A local variable "m" is used to store the inverse matrix in the
       ## current (i.e., associated to makeCacheMatrix) environment.
       ## Note that a different environment is created for every input matrix.
       ## On a explicit, direct call to makeCacheMatrix, m is first set
       ## to NULL and is later modified by setinverse().
       m <- NULL

       ## set: function to store the value of the matrix, argument is a matrix.
       ## This function is defined inside another function (makeCacheMatrix)
       ## and has an environment (specified by x), so it's a "function closure"
       set <- function(y) {
               ## Assign the input matrix to "x" in the parent
               ## (i.e., makeCacheMatrix) environment using the "<<-" operator
               x <<- y

               ## We just stored the matrix: the inverse is not available yet,
               ## so initialize it to NULL.
               m <<- NULL
       }

       ## get: function to get the matrix. It already knows the value of the
       ## matrix x from the environment, so no argument is needed.
       get <- function() {
               ## The matrix is read from the parent 
               ## (i.e., makeCacheMatrix) environment.
               x
       }

       ## setinverse: function to store the inverse of the matrix,
       ## argument is the inverse of the matrix.
       setinverse <- function(inverse) {
               ## Assign the inverse to "m" in the makeCacheMatrix environment
               m <<- inverse
       }

       ## getinverse: function to get the inverse of the matrix,
       ## no argument needed.
       getinverse <- function() {
               ## The inverse is read from the makeCacheMatrix environment.
               m
       }

       ## Return a "special matrix": a list of function closures to get
       ## and set the value of a matrix and its inverse (the latter is NULL
       ## if not available) plus an environment.
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



################
################
## cacheSolve: function that returns the inverse of a "special matrix" object,
## using cached data if available.
##
## First argument "x" is a "special matrix" object created with the function
## makeCacheMatrix(), remaining arguments "..." are passed to solve().

cacheSolve <- function(x, ...) {
        
       ## Get the inverse using the getinverse() function of the
       ## "special matrix" object "x", and store it in a local
       ## (i.e., defined in the cacheSolve environment) variable "m".
       m <- x$getinverse()

       ## Check if the inverse is cached (should not be NULL) 
       if(!is.null(m)) {
               ## The inverse is not NULL: it's cached, so just
               ## return it and end the execution of cacheSolve().
               message("getting cached data")
               return(m)
       } 

       ## The code below is executed if the inverse obtained with
       ## x$getinverse() is NULL: in that case, the inverse is not cached
       ## in the "special matrix" object "x", and must be calculated.
             
       ## Get the matrix using the get() function
       ## of the "special matrix" object "x".
       data <- x$get()

       ## Calculate the inverse of the matrix
       ## with R's standard solve() function.
       m <- solve(data, ...)

       ## Store the inverse in the cache using the setinverse()
       ## function of the "special matrix" object "x".
       x$setinverse(m)

       ## Return the inverse of the matrix. 
       return(m)
}
