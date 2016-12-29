# PROYECTO GRUPAL. Algoritmos bioinspirados
# Autores: Jagoba Perez & Aitor De Blas
# ----------------------------------------

# Establecemos el directorio de trabajo:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Leemos los datos con los que vamos a trabajar: por una parte "router.csv" contiene las características
# y el identificativo de cada uno de los routers, mientras que "topologia_red.csv" contiene las relaciones
# de cada uno de los routers con el resto de los routers.
datos_routers = read.csv("router.csv")
datos_topologia = read.csv("topologia_red.csv")

# Definimos e implementamos nuestro algoritmo genético en la siguiente función:
UDroutenizer=function(ROUTER_INICIO, ROUTER_DEST){
  N = dim(datos_routers)[1]; # Número de routers declarados en nuestros datos.
  PORC_IMPORTANCIA_RETARDO = 0.8 # Porcentaje de importarcia/relevancia del retardo (latency)
  PORC_IMPORTANCIA_VELPROC = 0.8 # Porcentaje de importancia/relevancia de la velocidad de procesamiento
  PORC_IMPORTANCIA_VELTRANS = 0.6 # Porcentaje de importancia/relevancia de la velocidad de transmisión
  PORC_IMPORTANCIA_BUFFER = 0.75 # Porcentaje de importancia/relevancia del tamaño del buffer
  ROUTER_DEST = 7; # TO DELETE!!!!!
  # Declaramos la función de normalización:
  normalize = function(x) {(x-min(x))/(max(x)-min(x))}
  # Normalizamos todos los datos que vamos a utilizar para el cálculo del INDICE ("distancia"):
  datos_routers[,c("Vel..Transmisión..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")] = as.data.frame(lapply(datos_routers[,c("Vel..Transmisión..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")], normalize))[,c("Vel..Transmisión..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")]
  datos_topologia[,c("Latencia..ms..")] = as.data.frame(lapply(datos_topologia, normalize))[, c("Latencia..ms..")]

  # PASO 1: Creamos una matriz de distancias que en nuestro caso contendra un valor numerico llamado "INDICE".
  # Este índice indica cuán ótimo es la relacion ("distancia") entre un router y otro.
  # Cuanto mayor sea el índice, entonces, mejor valorada está la distancia entre ambos routers.
  # Contra menor sea el índice, entonces, peor valorado estará la distancia entre ambos routers.
  INDICES = matrix(0,N,N);
  for (i in 1:dim(datos_topologia)[1]){
    current = datos_topologia[i,]
    datos_router_source = datos_routers[current$ID.de.Router,]
    datos_router_dest = datos_routers[current$ID.de.Router.Conectado,]
    indice_calculado = (datos_router_source$Vel..Transmisión..Mb.s.*PORC_IMPORTANCIA_VELTRANS) +
                        (datos_router_source$Vel..Procesamiento..Mb.s.*PORC_IMPORTANCIA_VELPROC) +
                        (datos_router_source$Buffer*PORC_IMPORTANCIA_BUFFER) -
                        (current$Latencia..ms..*PORC_IMPORTANCIA_RETARDO)
    INDICES[current$ID.de.Router,current$ID.de.Router.Conectado] = indice_calculado;
  }
  
  # remove(datos_routers_normalizados) # remove se utililza para liberar memoria
  
  # Parámetros de cara a las iteraciones de las generaciones:
  N_INDIVIDUOS  = 10;
  L_INDIVIDUO   = N;
  GENERACIONES  = 10;
  PROB_MUTACION = 0.05;
  PROB_CRUCE    = 0.90;
  
  # PASO 2: INICIALIZACIÓN. CREAR UNA MATRIZ LLAMADA <<POBLACIÓN>>, CON N_INDIVIDUOS FILAS Y L_INDIVIDUO COLUMNAS. 
  POBLACION = matrix(0,N_INDIVIDUOS,L_INDIVIDUO);
  for (i in 1:N_INDIVIDUOS){
    # Iniciar cada uno de los individuos de la población como una permutación aleatoria
    POBLACION[i,] = c(1,sample(c(datos_routers[-1,"ID.de.router"])));
  }
  
  for (g in 1:GENERACIONES){
    # EVALUACIÓN
    FITNESS = Evaluar(POBLACION,INDICES,ROUTER_DEST);      
    
    # PASO 3: SELECCIÓN 
    #Seleccionar N_INDIVIDUOS padres por torneo binario
    PADRES = POBLACION;     
    for (j in 1:N_INDIVIDUOS) {
      padres_candidatos = sample(1:N_INDIVIDUOS, 2, replace=FALSE, prob=NULL) # El numero de muestras que cogemos son 2 de entre los N_INDIVIDUOS padres posibles.
      if(FITNESS[padres_candidatos[1]] < FITNESS[padres_candidatos[2]]) { # Determinamos cual es aquel padre de los dos que ha sido evaluado con el valor mas bajo de la funcion FITNESS.
        PADRES[j,] = POBLACION[padres_candidatos[1],]
      } else {
        PADRES[j,] = POBLACION[padres_candidatos[2],]
      }
    }
    
    # PASO 4: CRUCE 
    #Para cada pareja de padres, usar el operador de orden con probabilidad PROB_CRUCE para generar dos hijos
    HIJOS = PADRES[,-1];
    valor_router_inicial = PADRES[1,1]
    PADRES = PADRES[,-1]
    for (i in seq(from=1, to=N_INDIVIDUOS, by=2)) { # iteramos de dos en dos, porque tenemos que coger dos padres y cruzarlos entre ellos.
      # Vamos a cruzar parejas de padres en base a la probabilidad de cruce. Si no se da el caso, entonces, no cruzamos:
      if (runif(1,min=0,max=1)<= PROB_CRUCE) {
        indices = sample(2:(L_INDIVIDUO-2), 2, replace = FALSE, prob = NULL) # Seleccionamos aleatoriamente dos indices de corte, tenemos que tener cuidado con que los indices no sean ni el 1 ni el ultimo. Porque si no, no habria tres trozos.
        indice_corte1 = min(indices)
        indice_corte2 = max(indices)
        HIJOS[i,] = cruce_de_orden(PADRES[i,], PADRES[i+1,], indice_corte1, indice_corte2)
        HIJOS[i+1,] = cruce_de_orden(PADRES[i+1,], PADRES[i,], indice_corte1, indice_corte2)
      } else {
        # Si no se da la probabilidad de cruce, entonces, dejamos los hijos tal y como estan los padres:
        HIJOS[i,] = PADRES[i,]
        HIJOS[i+1,] = PADRES[i+1,]
      }
    }
    
    # PASO 5: MUTACIÓN  
    #Para cada hijo, con probabilidad PROB_MUTACION, intercambiar dos posiciones elegidas aleatoriamente
    for(j in 1:N_INDIVIDUOS) {
      # Vamos a mutar cada hijo en base a la probabilidad de mutacion. Si no se da el caso, entonces, no mutamos:
      if (runif(1,min=0,max=1) <= PROB_MUTACION) {
        indices = sample(1:(L_INDIVIDUO-1), 2, replace=FALSE, prob=NULL) # Obtenemos dos indices (posiciones) los cuales vamos a usar para intercambiar sus respectivos valores.
        aux = HIJOS[j,indices[1]]
        HIJOS[j,indices[1]] = HIJOS[j,indices[2]]
        HIJOS[j,indices[2]] = aux
      } # Si no se da la probabilidad de mutacion, entonces, dejamos el hijo tal y como estaba
    }
    
    HIJOS = cbind(valor_router_inicial, HIJOS) # cbind(V1 = 1, HIJOS)
    PADRES = cbind(valor_router_inicial, PADRES)
    
    # ACTUALIZAMOS EL MEJOR INDIVIDUO (No Tocar)
    indice = order(FITNESS, decreasing = TRUE)[1];
    MEJOR = POBLACION[indice,];
    fitness_mejor = FITNESS[indice];
    
    # MOSTRAMOS EL MEJOR INDIVIDUO HASTA EL MOMENTO (No Tocar)
    print(paste0('Mejor Individuo Generación: ',g))
    print(MEJOR)
    print(paste0('Fitness del Mejor Individuo: ',fitness_mejor))
    
    # REEMPLAZAMIENTO (No Tocar)
    POBLACION = HIJOS;
    
    # PASO 5: ELITISMO 
    # En caso de que el MEJOR individuo no se encuentre en la población, introducirlo en ella
    esta = 0
    for (l in 1:N_INDIVIDUOS) {
      if (isTRUE(all.equal(MEJOR,POBLACION[l,]))) {esta = 1; break}
    }
    if (!esta) {POBLACION[N_INDIVIDUOS,] = MEJOR}
  }
  
}

Evaluar=function(POBLACION,INDICES, ROUTER_DEST){
  FITNESS=matrix(0,dim(POBLACION)[1],1);
  maxj = (dim(POBLACION)[2]-1);
  for (i in 1:dim(POBLACION)[1]){
    distancia_total = 0 
    for (j in 1:maxj){
      elem1 = POBLACION[i,j]
      elem2 = POBLACION[i,j+1]
      if (elem1 == ROUTER_DEST) {break;}
      dist = INDICES[elem1, elem2]
      if (dist == 0) {distancia_total = 0; break}
      distancia_total = distancia_total + dist
    }
    FITNESS[i]=distancia_total;
  }
  FITNESS
}

cruce_de_orden = function(PADRE1, PADRE2, indice1, indice2) {
  # Ahora cruzamos los trozos realizados del cromosoma. Situamos el trozo 3 en la primera posicion (al principio), el trozo 1 a coninuacion del que hemos colocado y finalmente el trozo 2 al final. Es al fin y al cabo, como una permutación de los trozos del cromosoma:
  primerCruce = c(PADRE1[(indice2+1):length(PADRE1)], PADRE1[1:indice1], PADRE1[(indice1+1):indice2]) 
  # Ahora, comprobamos si los elementos (valores) troncales (trozo 2 del padre 2!) se encuentran dentro del cromosoma que hemos cruzado. Si es asi, eliminamos esos valores de dentro del cromosoma:
  despuesDeResta = vector() # vector vacio al que anadiremos los valores que nos interesa mantener y no descartar.
  for (l in 1:length(primerCruce)) {
    if (!(primerCruce[l] %in% PADRE2[(indice1+1):indice2])) {despuesDeResta = c(despuesDeResta, primerCruce[l])} # En este caso lo estamos haciendo a la inversa, en vez de eliminarlos del vector, estamos creando uno nuevo y sumando los valores que no debemos eliminar.
  }
  # A continuacion, concatenamos los valores del trozo 2 del padre 2 con todo aquello que haya quedado del padre1:
  concatenacion = c(PADRE2[(indice1+1):indice2], despuesDeResta)
  # Todavia nos falta mover un ultimo trozo del cromosoma que estamos modificando.
  # Tenemos que mover un trozo compuesto por el mismo numero de genes que el trozo 1 del padre 2.
  # El trozo del que hablamos esta situado en la parte final del cromosoma. Al final es llevar genes de la parte final del cromosoma hacia la parte delantera del cromosoma:
  resultado = c(tail(concatenacion, n=indice1), concatenacion[1:(length(concatenacion)-indice1)])
  return(resultado)
}

