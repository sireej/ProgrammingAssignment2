## Calculates the inverse of a matrix and gives the cached data if done repeatedly

## stores the matrix and inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  
  inverse <- NULL
  set <- function(mat1) {
    mat <<- mat1
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Will calculate the inverse and store it 
## and if asked repeatedly will return the cached inverse

cacheSolve <- function(cacheinv, ...) {
        ## Return a matrix that is the inverse of 'cacheinv'
  
  inverse <- cacheinv$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- cacheinv$get()
  inverse <- solve(mat)
  cacheinv$setinverse(inverse)
  inverse  
}
