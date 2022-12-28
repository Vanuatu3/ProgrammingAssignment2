## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function makeCacheMatrix is made up of getminv, setminv, get and set and used to cache 
##the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y){
                x<<- y
                minv<- NULL
                }
get <- function()x
setminv <- function(inverse)minv<<- inverse
getminv <- function(){
                inver <- ginv(x)
                inver%%x
                }
list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function
## cached data retrieval 

cacheSolve <- function(x, ...) {
        ginv <- x$getinv()
        if(!is.null(ginv)) {                  #Null parameter 
                message("getting cached data")
                return(ginv)
        }
        data <- x$get()
        inv <- solve(data, ...)         #gets inverse value
        x$setinv(ginv)
        ginv                            #inverse of x matrix
}
      
