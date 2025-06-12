# XRD_sub_bkg

# Aplicación de Procesamiento de Archivos `.xy` para Difracción de Rayos X

Esta aplicación en R permite procesar múltiples archivos `.xy` de difracción de rayos X de forma interactiva, mediante una interfaz por consola paso a paso. Fue diseñada para facilitar el preprocesamiento de datos antes de análisis con paquetes como `PowdR`.
# Uso:
Descargar el archivo `XRD_xy_processing.R` y ejecutarlo.

## 🚀 Funcionalidades

El programa permite realizar los siguientes pasos en lote sobre todos los archivos `.xy` de una carpeta:

1. **Lectura robusta de archivos**  
   Lee archivos `.xy` con o sin encabezados tipo `*FILE_COMMENT`, eliminando líneas no numéricas.

2. **Sustracción de fondo (background)**  
   Usa el método `bkg()` del paquete `PowdR` para estimar y sustraer el fondo de los difractogramas.

3. **Alineamiento con patrón de cuarzo**  
   Permite alinear los difractogramas con un patrón de referencia con respecto al cuarzo.

4. **Recorte e interpolación de valores de tth**  
   Todos los archivos se recortan a un rango común de ángulo (`tth`) definido por el usuario y se interpolan a una grilla uniforme, asegurando que todos los archivos tengan el mismo número de puntos.

5. **Exportación con encabezado original**  
   Los archivos procesados se guardan en una carpeta de salida, incluyendo el encabezado original si existe.

---

## 💻 Uso

Descarga el archivo `XRD_xy_processing.R` y ejecutalo, luego ejecuta la función principal en R:


```r
procesar_batch_xy()
```
> [!WARNING]
> En caso de no tener instaladas las siguientes librerias, las debes instalar usando:
> `devtools::install_github('benmbutler/powdR')`
> `install.packages(c("tidyverse","reshape2"), )`


## Versión
- **v1.0**

## Autor
- **David Caro**  
- 📧 [decaroc@unal.edu.co](mailto:decaroc@unal.edu.co)

