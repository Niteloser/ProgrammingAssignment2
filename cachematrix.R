## Put comments here that give an overall description of what your
## functions do

## Functions to create a special "matrix" object, caching its inverse


## makeCacheMatrix create a list of functions to
## i) set the matrix to invert
## ii)get the matrix to invert
## iii) set the inverse matrix
## iv) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
       i<-NULL
       set<-function(y)
         {
          x<<-y
          i<<-NULL
         }
       get<-function() x
       setinv<-function(inv) i<<-inv
       getinv<-function() i
       list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve returns the inverse of a matrix. It first checks
## if the inverse has already been computed and if so returns
## the cached value. Otherwise it computes and caches the inverse
## of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv))
         {
          message("gettinc cached inverse")
          return(inv)
         }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinv(inv)
        inv
}
