## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
			n<-sqrt(length(x))
			x<-matrix(x,nrow=n,ncol=n,byrow=TRUE)
                  matinv <- NULL
                  setmat <- function(y){
                                x <<- y
					  n<-sqrt(length(x))
					  x<-matrix(x,nrow=n,ncol=n,byrow=TRUE)

                                matinv <<- NULL
                  
                    }
                    getmat <- function() x
                    setinv <- function(inv) matinv <<- inv
                    getinv <- function() matinv
                    list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		    matinv <- x$getinv()
                if(!is.null(matinv)){
                  message("getting cached data")
                  return(matinv)
                }
                  leng <- sqrt(length(x$getmat()))
			data <- matrix(x$getmat(),nrow=leng,ncol=leng,byrow=TRUE)

			matinv <- solve(data)
			x$setinv(matinv)
           		matinv
                }

