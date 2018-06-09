## Creates a vector (list) of functions to 1. Get matrix, 2. Set matrix, 
## 3. Get inverse, 4. Set inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function () inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates inverse of matrix. If inverse previously calculated,
## returns cached inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
