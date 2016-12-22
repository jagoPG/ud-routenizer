# PROYECTO GRUPAL. Algoritmos bioinspirados
# Autores: Jagoba Perez & Aitor De Blas
# ----------------------------------------

# Establecemos el directorio de trabajo:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Leemos los datos con los que vamos a trabajar: por una parte "router.csv" contiene las caracter�sticas
# y el identificativo de cada uno de los routers, mientras que "topologia_red.csv" contiene las relaciones
# de cada uno de los routers con el resto de los routers.
datos_routers = read.csv("router.csv")
datos_topologia = read.csv("topologia_red.csv")

# Definimos e implementamos nuestro algoritmo gen�tico en la siguiente funci�n:
UDroutenizer=function(){
  N = dim(datos_routers)[1]; # N�mero de routers declarados en nuestros datos.
  PORC_IMPORTANCIA_RETARDO = 0.8 # Porcentaje de importarcia/relevancia del retardo (latency)
  PORC_IMPORTANCIA_VELPROC = 0.8 # Porcentaje de importancia/relevancia de la velocidad de procesamiento
  PORC_IMPORTANCIA_VELTRANS = 0.6 # Porcentaje de importancia/relevancia de la velocidad de transmisi�n
  PORC_IMPORTANCIA_BUFFER = 0.75 # Porcentaje de importancia/relevancia del tama�o del buffer
  
  # Declaramos la funci�n de normalizaci�n:
  normalize = function(x) {(x-min(x))/(max(x)-min(x))}
  # Normalizamos todos los datos que vamos a utilizar para el c�lculo del INDICE ("distancia"):
  datos_routers[,c("Vel..Transmisi�n..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")] = as.data.frame(lapply(datos_routers[,c("Vel..Transmisi�n..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")], normalize))[,c("Vel..Transmisi�n..Mb.s.", "Vel..Procesamiento..Mb.s.", "Buffer")]
  datos_topologia[,c("Latencia..ms..")] = as.data.frame(lapply(datos_topologia, normalize))[, c("Latencia..ms..")]

  # PASO 1: Creamos una matriz de distancias que en nuestro caso contendra un valor numerico llamado "INDICE".
  # Este �ndice indica cu�n �timo es la relacion ("distancia") entre un router y otro.
  # Cuanto mayor sea el �ndice, entonces, mejor valorada est� la distancia entre ambos routers.
  # Contra menor sea el �ndice, entonces, peor valorado estar� la distancia entre ambos routers.
  INDICES = matrix(0,N,N);
  for (i in 1:dim(datos_topologia)[1]){
    current = datos_topologia[i,]
    datos_router_source = datos_routers[current$ID.de.Router,]
    datos_router_dest = datos_routers[current$ID.de.Router.Conectado,]
    indice_calculado = (datos_router_source$Vel..Transmisi�n..Mb.s.*PORC_IMPORTANCIA_VELTRANS) +
                        (datos_router_source$Vel..Procesamiento..Mb.s.*PORC_IMPORTANCIA_VELPROC) +
                        (datos_router_source$Buffer*PORC_IMPORTANCIA_BUFFER) -
                        (current$Latencia..ms..*PORC_IMPORTANCIA_RETARDO)
    INDICES[current$ID.de.Router,current$ID.de.Router.Conectado] = indice_calculado;
  }
  
  # remove(datos_routers_normalizados) # remove se utililza para liberar memoria
  
  # Par�metros de cara a las iteraciones de las generaciones:
  N_INDIVIDUOS  = 50;
  L_INDIVIDUO   = N;
  GENERACIONES  = 2000;
  PROB_MUTACION = 0.05;
  PROB_CRUCE    = 0.90;
  
  # PASO 2: INICIALIZACI�N. CREAR UNA MATRIZ LLAMADA <<POBLACI�N>>, CON N_INDIVIDUOS FILAS Y L_INDIVIDUO COLUMNAS. 
  POBLACION = matrix(0,N_INDIVIDUOS,L_INDIVIDUO);
  for (i in 1:N_INDIVIDUOS){
    # Iniciar cada uno de los individuos de la poblaci�n 
    # como una permutaci�n aleatoria
    for (j in 1:L_INDIVIDUO){
      POBLACION[i,j] = j;
    }
  }
  POBLACION
  
  for (g in 1:GENERACIONES){
    # EVALUACI�N (No Tocar)
    FITNESS = Evaluar(POBLACION,DISTANCIAS);      
    
    # PASO 3: SELECCI�N 
    #Seleccionar N_INDIVIDUOS padres por torneo binario
    PADRES = POBLACION;     
    
    # PASO 4: CRUCE 
    #Para cada pareja de padres, usar el operador de orden con probabilidad PROB_CRUCE para generar dos hijos
    HIJOS = PADRES;    
    
    # PASO 5: MUTACI�N  
    #Para cada hijo, con probabilidad PROB_MUTACION, intercambiar dos posiciones elegidas aleatoriamente
    
    
    # ACTUALIZAMOS EL MEJOR INDIVIDUO (No Tocar)
    indice = order(FITNESS)[1];
    MEJOR = POBLACION[indice,];
    fitness_mejor = FITNESS[indice];
    
    # MOSTRAMOS EL MEJOR INDIVIDUO HASTA EL MOMENTO (No Tocar)
    print(paste0('Mejor Individuo Generaci?n: ',g))
    print(MEJOR)
    print(paste0('Fitness del Mejor Individuo: ',fitness_mejor))
    
    # REEMPLAZAMIENTO (No Tocar)
    POBLACION = HIJOS;
    
    # PASO 5: ELITISMO 
    # En caso de que el MEJOR individuo no se encuentre en la poblaci�n, introducirlo en ella
  }
  
}

Evaluar=function(POBLACION,DISTANCIAS){
  FITNESS=matrix(0,dim(POBLACION)[1],1);
  maxj = (dim(POBLACION)[2]-1);
  for (i in 1:dim(POBLACION)[1]){
    for (j in 1:maxj){
      FITNESS[i]=FITNESS[i]+DISTANCIAS[POBLACION[i,j],POBLACION[i,j+1]];
    }
    FITNESS[i]=FITNESS[i]+DISTANCIAS[POBLACION[i,dim(POBLACION)[2]],POBLACION[i,1]];
  }
  FITNESS
}