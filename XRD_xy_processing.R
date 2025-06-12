devtools::install_github('benmbutler/powdR')
install.packages(c("tidyverse","reshape2"), )
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

#--- Función para crear el archivo matriz

<<<<<<< HEAD
xy_matriz_csv <- function(sub_bkg_dir){
  sub_bkg_files <- dir(sub_bkg_dir, pattern = "\\.xy$", full.names = TRUE)
  
  multi_data <- read_xy(sub_bkg_files,header = TRUE)
  # Strip each data.frame to only tth and counts
  cleaned_data <- lapply(multi_data, function(df) df[, c("tth", "counts")])
}

#--- Función principal 

=======
>>>>>>> 4bbfad31c6d99073c983dae9f0a7b01734d264f5
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
        if (max(xy_file_bkg_sub$tth) > 60) {
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
                  min(xy_file_bkg_sub$tth), "a: ", max(xy_file_bkg_sub$tth)))
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
  
  cat("✅ Proceso finalizado.\n")
}

