## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Función que crea una matrix especial que puede 'almacenar' su matriz inversa.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv<- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)


}


## Función que calcula la inversa de la matriz especial creada con la función anterior. Si la inversa fue calculada
## y la matriz no cambia entonces nos devuelve el valor de la matriz inversa que se tenía almacenda.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv

}
