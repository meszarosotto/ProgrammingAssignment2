## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates an object with methods for implement cache  

  makeCacheMatrix <- function(x = matrix()) {
    ## intialize the inverse matrix
    inver <- NULL
    ## set and get methods
    set <- function(a_matrix) {
      x <<- a_matrix
      inver <<- NULL
    }
    get <- function() x
    # set/getinverse methods
    setinverse <- function(a_inver) inver <<- a_inver
    getinverse <- function() inver
    
    # list of matrix elements
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
  }

## the cacheSolve function calculates inverse matrix of any matrix which has a solution 

cacheSolve <- function(x, ...) {
  
  i_matrix<-x$getinverse()
  ## check if the matrix's inverse already calculated or not
  if(!is.null(i_matrix)) {
    message("getting cached data")
    return(i_matrix)
  }
  data<-x$get()
  ## see determinant to check if solution exist or not
    if(det(data)!=0){
    i_matrix<-solve(data, ...)
    x$setinverse(i_matrix)
    return(i_matrix)
  }
  message("matrix has no inverse")
}
