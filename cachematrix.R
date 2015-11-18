## The following functions work together to make repeated calculations of the inverse of a matrix less resource intensive. This is accomplished by caching the value of the matrix inverse for use in future calculations

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #resets inverse value to NULL
  set<-function(y){ #function that changes value stored in the main function
    x<<-y
    m<<-NULL
  }
  get<-function() x #function retrieving matrix x stored in main function
  setinverse<-function(solve) m <<-solve  #stores input value in main function
  getinverse<-function() m   #returns input value in main function
  list(set = set,get = get, setinverse = setinverse, #lists stored functions for retrieval
  getinverse = getinverse)
}


## cacheSolve computes the inverse of the object returned by makeCacheMatrix, using the cached value if it had been previously calculated and remains unchanged. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){   #if inverse value is already cached, displays value and does not recompute
    message("getting cached data")
    return(m)
  }              #if no value exists, compute inverse and stores in makeCache object
  data<-x$get()
  m<-solve(data,...) #computes inverse
  x$setinverse(m)   #stores value for later retrieval
  m
}


mat<-matrix(1:4,nrow=2,ncol=2)
test<-makeCacheMatrix(mat)
cacheSolve(test)

