## makeCacheMatrix is creating an R object that stores a matrix and its inverse
## cacheSolve requires an arguement that is returned by makeCacheMatrix in order
## to retrieve the inverse from the cache value that is stored in the 
## makeCacheMatrix objects environment
## first you must make a square matrix and store it in x i.e. x<-matrix(1:4,2)
## Then you can run makeCacheMatrix of x and store in another object
## Then run Cachesolve with that object i.e. a<-makeCacheMatrix(x) 
## >cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
              mat <- NULL    #setting the value of the matrix to NULL
              set<-function(y) {
                x<<- y     #assigns a value of y to x in higher environment
                mat<<- NULL  #assigns NULL to mat in higher environment
              }
              get <- function()x #get the value of x
              setinverse <- function(inverse) mat<<-inverse 
              getinverse <- function() mat
              list(set=set,
                   get=get,
                   setinverse=setinverse,
                   getinverse=getinverse)  
              }
              

## This function will return the inverse of the matrix defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)){
          message("getting cached data")
          return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
        ## Return a matrix that is the inverse of 'x'
}
