## This is my attempt at defining the function that creates a special matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y) {
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) i<<-inverse
    getinverse<-function() i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## THis is my attempt at creating a function that computes the inverse and set(or get) value to/from the cache
cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
}
