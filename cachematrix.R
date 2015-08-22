
 
  
  makeCacheMatrix <- function(x = matrix()) {

    ##  function to set and get the matrix
    ##  set the inverse and get 
    ##  source acknowledged includes material from GUANGMING LANG on http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
    
    myinverse = NULL
    set = function(y) {
      # use `<<-` to assign a value to an object in an environment 
      # different from the current environment. 
      x <<- y
      # set to NULL
      myinverse <<- NULL
    }
    get = function() x
    setinv = function(inverse) myinverse <<- inverse 
    getinv = function() myinverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }
  
  cacheSolve <- function(x, ...) {
   
    myinverse = x$getinv()
    
    # if already done
    if (!is.null(myinverse)){
      # then get from cache 
      message("getting cached data")
      return(myinverse)
    }
    
    # else inverse
    mat.data = x$get()
    myinverse = solve(mat.data, ...)
    
    # sets the inverse
    x$setinv(myinverse)
    
    return(myinverse)
  }