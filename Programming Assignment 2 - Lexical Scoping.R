#make Cache Matrix function 
makeCacheMatrix <- function(x = matrix()) 
  inv <- NULL
  set <- function(y)
    x <<- y
    inv <<- NULL
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

#cache Solve function 
  cacheSolve <- function(x, ...) 
    inv <- x$getInverse()
    if(!is.null(inv))
      message("getting cached data")
      return(inv)
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      

  