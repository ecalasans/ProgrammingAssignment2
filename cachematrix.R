## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Incializa inversa como nula
        invMatriz <- NULL
        
        # Inicializa cache da matriz
        setMatriz <- function(m){
                x <<- m
                invMatriz <<- NULL
        }
        
        # Retorna cache
        getMatriz <- function() x
        
        # Incializa/Atualiza o cache da inversa
        setInversa <- function(inversa) invMatriz <<- inversa
        
        # Retorca cache da inversa
        getInversa <- function() invMatriz
        
        # Lista de funcoes
        list(setMatriz = setMatriz, getMatriz = getMatriz, 
             setInversa = setInversa, getInversa = getInversa)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Obtem inversa do cache
        M <- x$getMatriz()
        
        # Se nao houver inversa, retorna a matriz
        if(is.null(x$getInversa())){
                message("There's no inverse matrix calculated yet!  Returning matrix.")
                return(M)
        }
        
        # Calcula inversa
        invertida <- solve(M)
        
        # Atualiza cache
        x$setInversa(invertida)
        
        invertida
        
}
