## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    if(det(x) == 0){ # singular matrix
        message('Singular, no Inversion')
        return(NULL)
    }
    set <- function(newmat){
        x <<- newmat
        mat <<- NULL
    }
    get <- function() x
    setInversion <- function(inver) mat <- inver
    getInversion <- function() mat
    list(get = get, set = set, getInver = getInversion,setInver = setInversion)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInver()
    if(!is.null(m)){
        # inversion not calculated or
        # is a singular
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInver(m)
    m;x$getInver()
}
