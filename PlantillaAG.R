setwd('C:\\Users\\Administrador.WIN-FC1VBLJ76B5\\Google Drive\\2016 06 Inteligencia Artificial Avanzada 2016-17\\Materiales\\Codigo')

PlantillaAGv2=function(){
  # LEEMOS LAS POSICIONES DE REPARTO, Y CALCULAMOS LAS DISTANCIAS (No Tocar)
  PROBLEMA = 'berlin52.txt';
  SOLUCION = 'berlin52.opt.txt';
  PUNTOS = read.table(PROBLEMA);
  OPTIMO = read.table(SOLUCION);
  N = dim(PUNTOS)[1];
  
  # PASO 1: CREAR UNA MATRIZ LLAMADA <<DISTANCIAS>>, DE NxN ELEMENTOS. EN CADA POSICI?N DEBE ALMACENAR LA DISTANCIA ENTRE 2 CIUDADES
  # DISTANCIAS[i,j] <- distancia eucl?dea entre PUNTOS[i,] y PUNTOS[j,]
  DISTANCIAS = matrix(0,N,N);
  for (i in 1:N){
    for (j in 1:N){   
      DISTANCIAS[i,j] = 0;
    }
  }
  
  
  # PAR?METROS
  N_INDIVIDUOS  = 50;
  L_INDIVIDUO   = N;
  GENERACIONES  = 2000;
  PROB_MUTACION = 0.05;
  PROB_CRUCE    = 0.90;
  
  
  # PASO 2: INICIALIZACI?N. CREAR UNA MATRIZ LLAMADA <<POBLACI?N>>, CON N_INDIVIDUOS FILAS Y L_INDIVIDUO COLUMNAS. 
  POBLACION = matrix(0,N_INDIVIDUOS,L_INDIVIDUO);
  for (i in 1:N_INDIVIDUOS){
    # Iniciar cada uno de los individuos de la poblaci?n 
    # como una permutaci?n aleatoria
    for (j in 1:L_INDIVIDUO){
      POBLACION[i,j] = j;
    }
  }
  POBLACION
  
  for (g in 1:GENERACIONES){
    # EVALUACI?N (No Tocar)
    FITNESS = Evaluar(POBLACION,DISTANCIAS);      
    
    # PASO 3: SELECCI?N 
    #Seleccionar N_INDIVIDUOS padres por torneo binario
    PADRES = POBLACION;     
    
    # PASO 4: CRUCE 
    #Para cada pareja de padres, usar el operador de orden con probabilidad PROB_CRUCE para generar dos hijos
    HIJOS = PADRES;    
    
    # PASO 5: MUTACI?N  
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
    # En caso de que el MEJOR individuo no se encuentre en la poblaci?n, introducirlo en ella
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