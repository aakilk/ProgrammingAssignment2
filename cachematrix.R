## Creates a matrix that caches its inverse
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the matrix inverse
# 4. Get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
# 1. Check if matrix has been computed
# 2. If it has then get the result and skip the computation
# 3. If not, compute the inverse and set value in the cache using setinverse
cacheSolve <- function(x , ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Run Example:
## > x = diag (5, 3)
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    5    0    0
## [2,]    0    5    0
## [3,]    0    0    5

## No Cache:
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2

## Cached Solve:
## getting cached data.
## [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2