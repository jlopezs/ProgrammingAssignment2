################
################
## R programming assignment 2 
## JLS, Dec 2014

################
################
## makeCacheMatrix: create a "special matrix": a list object to set and get
##+the value of a matrix and its inverse (NULL if not available).
##
## argument is a matrix, e.g. inputMatrix<-matrix(c(2,0,0,2),nrow=2,ncol=2).
##
## to set the data inside a "special matrix" named sMatrix use
##     sMatrix$set(inputMatrix)           : store a matrix
##     sMatrix$setinv(solve(inputMatrix)) : store the inverse of the matrix
##
## to get the data inside sMatrix use
##     sMatrix$get()        : returns the original matrix
##     sMatrix$getinverse() : returns the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

       ## a local variable "m" is used to store the inverse matrix
       ##+in the current (i.e., associated to makeCacheMatrix) environment.
       ## note that a different environment is created for every input matrix.
       ## on a explicit, direct call to makeCacheMatrix, m is first set
       ##+to NULL and it is later modified by setinverse().
       m <- NULL

       ## function to store the value of the matrix
       ## argument is a matrix
       set <- function(y) {
               ## assign the input matrix to "x" in the parent
               ##+(i.e., makeCacheMatrix) environment.
               x <<- y

               ## we just stored the matrix: the inverse is not available yet.
               m <<- NULL
       }

       ## function to get the matrix, no argument needed
       get <- function() {
               ## the matrix is read from the parent 
               ##+(i.e., makeCacheMatrix) environment.
               x
       }

       ## function to store the inverse of the matrix.
       ## argument is the inverse of the matrix.
       setinverse <- function(inverse) {
               ## assign the inverse to "m" in the makeCacheMatrix environment
               m <<- inverse
       }

       ## function to get the inverse of the matrix, no argument needed.
       getinverse <- function() {
               ## the inverse is read from the makeCacheMatrix environment.
               m
       }

       ## return a "special matrix": a list with functions to get and set
       ##+the value of a matrix and its inverse (NULL if not available).
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



################
################
## cacheSolve: return the inverse of a "special matrix" object,
##+using cached data if available.
##
## first argument "x" is a "special matrix" object created with 
##+makeCacheMatrix(), remaining arguments "..." are passed to solve().

cacheSolve <- function(x, ...) {
        
       ## get the inverse using the getinverse() function of the
       ##+"special matrix" object "x", and store it in a local
       ##+(i.e., defined in the cacheSolve environment) variable "m".
       m <- x$getinverse()

       ## check if the inverse is cached (not NULL) 
       if(!is.null(m)) {
               ## the inverse is not NULL: it's cached, so just
               ##+return it and end the execution of cacheSolve().
               message("getting cached data")
               return(m)
       } 

       ## the code below is executed if the inverse is NULL: 
       ##+the inverse is not cached in the "special matrix"
       ##+object "x" and must be calculated.
             
       ## get the matrix using the get() function
       ##+of the "special matrix" object "x".
       data <- x$get()

       ## calculate the inverse of the matrix
       ##+with R's standard solve() function.
       m <- solve(data, ...)

       ## store the inverse in the cache using the setinverse()
       ##+function of the "special matrix" object "x".
       x$setinverse(m)

       ## Return the inverse of the matrix. 
       return(m)
}
