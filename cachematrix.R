## These functions will calculate and cache the inverse of a matrix.

## Creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) i<<-inverse
    getInverse<-function()i
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Calculates the inverse of the matrix created in makeCacheMatrix, and returns said inverse.

cacheSolve <- function(x, ...) {
        i<-x$getInverse()
        if(!is.null(i)){
            message("getting cached data")
            return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setInverse(i)
        i
}


   