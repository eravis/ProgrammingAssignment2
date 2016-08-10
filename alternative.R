 makeMatrix <- function(origmatrix = matrix()) {
            invmatrix <- NULL
            set <- function(y) {
                    origmatrix <<- y
                    invmatrix <<- NULL
            }
            
            
            get <- function() origmatrix
            setinvmatrix <- function(inversematrix) invmatrix <- inversematrix
              
            getinvmatrix <- function() invmatrix
          
                       
            list(set = set, get = get,
                 setinvmatrix = setinvmatrix,
                 getinvmatrix = getinvmatrix)
    }

  cachematrix <- function(origmatrix, ...) {
            invmatrix<- origmatrix$getinvmatrix()
          
            if(!is.null(invmatrix)) {
                    message("getting cached data")
                    return(invmatrix)
            }
            
            
            matr <- origmatrix$get()          
            invmatrix <- solve(matr, ...)
            origmatrix$setinvmatrix(invmatrix)
            return(invmatrix)
            
            
    }

testinvmatrix <- function(mat){
        ## mat: a matrix
        
        temp = makeMatrix(mat)
        start.time = Sys.time()
        cachematrix(temp)
        dur = Sys.time() - start.time
        print(dur)
        
    
        start.time = Sys.time()
        cachematrix(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        
}




set.seed(12222)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
