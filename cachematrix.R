## Funcao para calcular a matriz inversa e funcao para armazenar a matriz inversa em cache

## Essa funcao armazena em cache a matriz inversa

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversa <- function(inverse) inversa <<- inverse
        getinversa <- function() inversa
        list(set = set, get = get,
             setinversa = setinversa,
             getinversa = getinversa)

}


## Calculo da matriz inversa. Caso a matriz inversa ja tenha sido calculada, a funcao deve recuperar a matriz inversa salva em cache

cacheSolve <- function(x, ...) {
        ## Retorna uma matriz que e o inverso de 'x'
        inversa <- x$getinversa()
        if(!is.null(inversa)) {
                message("obtendo dados em cache")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinversa(inversa)
        inversa
        
}
