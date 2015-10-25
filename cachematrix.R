## Las funciones siguientes crean una matriz inversa
## y hacen que esté disponible en entorno caché

## > source("cachematrix.R")    Carga Programa R
## > a <- makeCacheMatrix()     Crea función matriz inversa
## > a$set(matrix(1:4, 2, 2))   Crea matriz en entorno de trabajo
## > cacheSolve(a)              Devuelve matriz inversa desde el entorno de trabajo
##                              Devuelve matriz inversa desde caché

makeCacheMatrix <- function(x = matrix()) {
        # almacena el valor en caché con valor por defecto NULL        
        cache <- NULL
        # crea la mtriz en el entorno de trabajo
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        # asigna el valor de la matriz
        get <- function() x
        # almacena en caché la matriz invertida
        setMatrix <- function(inverse) cache <<- inverse
        # asigna la matriz inversa desde el caché
        getInverse <- function() cache
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

## cacheSolve calcula la inversa de la matriz creada en makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## extrae la inversa de la matriz almacenada en caché
        cache <- x$getInverse()
        # devuelve la matriz inversa desde caché
        # o crea la matriz en el entorno de trabajo
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        matrix <- x$get()
        tryCatch( {
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )
        # muestra la matrix en consola
        return (cache)
}
