makeCacheMatrix <- function(x = matrix()) {
        ##
        ## This function creates a special "matrix" object   
        ## that can cache its inverse.
        ## SOLVE is used to create the inverse matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list( set = set
              , get = get
              , setinverse = setinverse
              , getinverse = getinverse
        )
}


cacheSolve <- function(x, ...) {
        ##
        ## This function computes the inverse of the special
        ## "matrix" returned by `makeCacheMatrix` above. If the inverse has
        ## already been calculated (and the matrix has not changed), then
        ##`cacheSolve` should retrieve the inverse from the cache.
        ##
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

