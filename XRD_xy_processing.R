
#devtools::install_github('benmbutler/powdR')
#install.packages(c("tidyverse","reshape2"), )

library(powdR)
library(tidyverse)
library(dplyr)
library(reshape2)

#---Función de lectura de xy
#La función toma los archivos que no funcionan y los limpia
read_clean_xy <- function(archivo_entrada) {
  # 1. Read all lines from the file
  lines <- readLines(archivo_entrada, warn = FALSE)
  
  # 2. Remove leading/trailing whitespace from each line
  lines <- trimws(lines)
  
  # 3. Remove lines that start with '*'
  clean_lines <- lines[!grepl("^\\*", lines)]
  
  # 4. Loop through each line and:
  #    a. Split it by whitespace
  #    b. Convert to numeric
  #    c. Keep only if it has exactly 2 numeric values
  clean_data <- lapply(clean_lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]  # Split by spaces or tabs
    nums <- suppressWarnings(as.numeric(parts))  # Convert to numeric, suppress warnings
    if (length(nums) == 2 && all(!is.na(nums))) {
      return(nums)  # Valid row: return numeric vector
    } else {
      return(NULL)  # Invalid row: discard
    }
  })
  
  # 5. Remove NULLs (invalid or non-numeric rows)
  clean_data <- Filter(Negate(is.null), clean_data)
  
  # 6. Combine all valid rows into a data frame
  mat <- do.call(rbind, clean_data)
  df <- as.data.frame(mat, stringsAsFactors = FALSE, col.names = FALSE)
  
  # 7. convert the data frame to xy object
  xy <- as_xy(df)
  
  # 8. Return the final cleaned xy object
  return(xy)
}
# Attempt to read a file with fallback
safe_read_xy <- function(archivo_entrada) {
  tryCatch({
    # Try PowdR's read_xy()
    df <- read_xy(archivo_entrada, header = TRUE)
    message("Successfully read using read_xy()")
    return(df)
  }, error = function(e) {
    message("read_xy() failed, using read_clean_xy() instead.")
    return(read_clean_xy(archivo_entrada))
  })
}
#Importar los datos de quartz para linear datos
data(minerals)
quartz <- data.frame(tth = minerals$tth, counts = minerals$xrd$QUA.1)

#---Función de Ayuda: ordenar_archivos_por_numero
# Esta función ordena las rutas de archivo numéricamente, extrayendo el primer número.
ordenar_archivos_por_numero <- function(lista_rutas_archivos) {
  nombres_base <- basename(lista_rutas_archivos)
  # Extrae el primer conjunto de dígitos del nombre del archivo (ej., "1" de "1.xy", "10" de "10_C.xy")
  numeros <- as.numeric(gsub("^.*?([0-9]+).*$", "\\1", nombres_base))
  return(lista_rutas_archivos[order(numeros)])
}

#--- Función para crear el archivo matriz

matriz_xrd_data <- function(input_folder_path, output_folder_path) {
  
  message(paste("Iniciando procesamiento de archivos desde:", input_folder_path))
  
  # --- Carga y Ordenamiento de Archivos ---
  # Obtiene la lista de todos los archivos .xy en la carpeta de entrada
  sub_bkg_files <- list.files(input_folder_path, pattern = "\\.xy$", full.names = TRUE)
  
  if (length(sub_bkg_files) == 0) {
    stop(paste("No se encontraron archivos .xy en la carpeta:", input_folder_path))
  }
  
  # Ordena los archivos numéricamente para asegurar un orden consistente de las muestras
  archivos_ordenados <- ordenar_archivos_por_numero(sub_bkg_files)
  
  # Carga los archivos .xy usando 'read_xy'.
  multi_data_raw <- read_xy(archivos_ordenados, header = TRUE)
  
  # --- Limpieza y Estandarización de DataFrames Individuales ---
  # Procesa cada dataframe: selecciona 'tth' y 'counts', y maneja problemas de encabezado/no-numéricos.
  cleaned_data <- lapply(multi_data, function(df) df[, c("tth", "counts")])

  # --- Construcción de la Matriz Final ---
  # La matriz tendrá: Filas = Muestras, Columnas = Valores tth, Contenido = Counts.
  
  # Paso 1: Extraer los valores de 'tth' comunes.
  # Como todos los 'tth' son idénticos y de la misma longitud, tomamos el del primer dataframe.
  tth_values <- cleaned_data[[1]]$tth
  
  # Paso 2: Crear una lista conteniendo solo los vectores de 'counts' de cada dataframe.
  counts_vectors_list <- lapply(cleaned_data, function(df) df$counts)
  
  # Paso 3: Obtener los nombres de las muestras.
  # Se basan en los nombres de archivo ordenados (ej., "1", "2", "1_C", "10").
  sample_names <- gsub("\\.xy$", "", basename(archivos_ordenados))
  
  # Paso 4: Combinar los vectores de 'counts' en una matriz.
  # 'do.call(rbind, ...)' apilará cada vector de counts como una fila.
  final_counts_matrix <- do.call(rbind, counts_vectors_list)
  
  # Paso 5: Asignar nombres a las filas y columnas de la matriz.
  # Las filas representarán las muestras.
  rownames(final_counts_matrix) <- sample_names 
  # Las columnas representarán los valores de tth.
  colnames(final_counts_matrix) <- tth_values   
  
  message("\n--- Matriz Final Creada (Muestras en Filas, tth en Columnas) ---")
  message(paste("Dimensiones de la matriz final:", nrow(final_counts_matrix), "filas (muestras) x", ncol(final_counts_matrix), "columnas (tth)"))
  
  # --- Conversión a Dataframe y Guardado a CSV ---
  # Imprime las primeras 5 filas y 5 columnas de la matriz para verificar
  counts_df_final <- as.data.frame(final_counts_matrix)
  
  message("\n--- Vista previa del Dataframe Final (primeras 5 columnas) ---")
  print(head(counts_df_final[, 1:min(5, ncol(counts_df_final))]))
  
  # Construye la ruta completa para el archivo de salida.
  output_file_path <- file.path(output_folder_path, "matriz_muestras.csv")
  
  # Asegura que la carpeta de salida exista. Si no, la crea.
  if (!dir.exists(output_folder_path)) {
    dir.create(output_folder_path, recursive = TRUE)
    message(paste("Carpeta de salida creada:", output_folder_path))
  }
  
  # Guarda el dataframe final como un archivo CSV.
  write.csv(counts_df_final, output_file_path)
  
  message(paste("\nArchivo CSV guardado exitosamente en:", output_file_path))
}


#--- Función principal 

procesar_batch_xy <- function(){
  cat("=== Procesamiento interactivo de archivos .xy ===\n\n")
  
  # 1. Pedir carpeta de entrada
  carpeta_entrada <- readline("📂 Ingresa la ruta a la carpeta de archivos .xy: ")
  if (!dir.exists(carpeta_entrada)) stop("La carpeta de entrada no existe.")
  
  # 2. Pedir carpeta de salida
  carpeta_salida <- readline("💾 Ingresa la ruta a la carpeta de salida: ")
  if (!dir.exists(carpeta_salida)) dir.create(carpeta_salida, recursive = TRUE)
  
  # 3. ¿Usar sustracción de fondo?
  usar_bkg <- tolower(readline("¿Deseas sustraer el background? (s/n): ")) == "s"
  
  # 4. ¿Usar alineamiento con cuarzo?
  usar_alineacion <- tolower(readline("¿Deseas alinear en X con respecto a cuarzo? (s/n): ")) == "s"
  
  # 5. Rango de recorte
  recorte <- tolower(readline("¿Deseas recortar en el eje X? (s/n): ")) == "s"
  if (recorte) {
    xmin <- as.numeric(readline("📏 Valor mínimo de tth (ej. 1): "))
    xmax <- as.numeric(readline("📏 Valor máximo de tth (ej. 40): "))
  }
  
  # 6. Listar archivos
  archivos_xy <- list.files(carpeta_entrada, pattern = "\\.xy$", full.names = TRUE)
  if (length(archivos_xy) == 0) stop("No se encontraron archivos .xy en la carpeta.")
  
  cat("\n🔄 Procesando archivos...\n\n")
  
  for (archivo_entrada in archivos_xy) {
    cat(paste("Procesando:", basename(archivo_entrada), "\n"))
    
    tryCatch({
      # Leer header
      lineas_archivo <- readLines(archivo_entrada)
      header_original <- head(lineas_archivo, 1)
      
      # Leer archivo y alinear si se desea
      xy_file <- safe_read_xy(archivo_entrada)
      if (usar_alineacion) {
        cat("Alineado respecto al Cuarzo.\n")
        if (max(xy_file$tth) > 60) {
          xmax_align <- 60
        } else {
          xmax_align <- max(xy_file_bkg_sub$tth)
        }
        xy_file <- align_xy(xy_file, std = quartz, xmin = 10, xmax = xmax_align, xshift = 0.2)
      }
      
      # Sustraer background si aplica
      if (usar_bkg) {
        xy_file_bkg <- bkg(xy_file)
        xy_file_bkg_sub_raw <- data.frame("tth" = xy_file_bkg$tth, "counts" = xy_file_bkg$counts - xy_file_bkg$background)
        xy_file_bkg_sub <- as_xy(xy_file_bkg_sub_raw)
        cat("Background sustraido con exito.\n")
      } else {
        xy_file_bkg_sub <- as_xy(data.frame("tth" = xy_file$tth, "counts" = xy_file$counts))
      }
      
      
      # Corregir valores negativos
      min_intensidad <- min(xy_file_bkg_sub$counts)
      if (min_intensidad < 0) {
        ajuste <- abs(min_intensidad)
        xy_file_bkg_sub$counts <- xy_file_bkg_sub$counts + ajuste
        cat(paste("  Se ajustaron", ajuste, "unidades para eliminar valores negativos.\n"))
      }
      
      # Recorte e interpolación
      if (recorte){
        xy_final <- xy_file_bkg_sub[xy_file_bkg_sub$tth >= xmin & xy_file_bkg_sub$tth <= xmax, ]
      } else {
        cat(paste("El archivo no se recortó en x, sus valores en tth van de: ", 
                  min(xy_file_bkg_sub$tth), "a: ", max(xy_file_bkg_sub$tth), "\n"))
        xy_final <- xy_file_bkg_sub
      }
      
      # Guardar archivo
      nombre_base <- tools::file_path_sans_ext(basename(archivo_entrada))
      archivo_salida <- file.path(carpeta_salida, paste0(nombre_base, ".xy"))
      writeLines(header_original, con = archivo_salida)
      write.table(xy_final,
                  file = archivo_salida,
                  sep = " ",
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE,
                  append = TRUE)
      
      
      cat(paste("Archivo procesado y guardado como:", basename(archivo_salida), "\n\n"))
      cat("✅ Archivo procesado exitosamente.\n\n")
      
    }, error = function(e) {
      cat("❌ Error al procesar:", basename(archivo_entrada), "\n")
      cat("   Detalles:", e$message, "\n\n")
    })
  }
  
  if(recorte){
    matriz_xrd_data(carpeta_entrada, carpeta_salida)
  }
  
  cat("✅ Proceso finalizado.\n")
}

