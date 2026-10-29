#' Procesa las salidas de Stock Synthesis
#'
#' Lee las salidas de un modelo con [r4ss::SS_output()], prepara los indicadores,
#' calcula intervalos de confianza, genera la figura 2 x 2 y guarda los archivos
#' resultantes en una carpeta dentro del directorio del modelo.
#'
#' @param dir Ruta al directorio que contiene las salidas de Stock Synthesis.
#' @param carpeta_salida Nombre de la subcarpeta donde se guardarán los archivos.
#'   Por defecto, `"adicionales"`.
#' @param area Área que se utilizará para extraer la biomasa total. Por defecto,
#'   `1`.
#' @param temporada Temporada que se utilizará para extraer la biomasa total.
#'   Por defecto, `1`.
#' @param referencia_ssb Valor opcional de la línea horizontal de referencia
#'   para biomasa desovante, en miles de toneladas. Por defecto, `NULL`.
#' @param referencia_F Valor opcional de la línea horizontal de referencia de
#'   mortalidad por pesca, en año⁻¹. Por defecto, `1`.
#'
#' @return Una lista, invisible, con la salida de Stock Synthesis, las tablas
#'   procesadas, los datos de la figura y las rutas de los archivos generados.
#'
#' @export
procesar_salidas_ss <- function(
    dir,
    carpeta_salida = "adicionales",
    area = 1,
    temporada = 1,
    referencia_ssb = NULL,
    referencia_F = 1
) {
  if (length(dir) != 1L || !dir.exists(dir)) {
    stop("`dir` debe ser la ruta a un directorio existente.", call. = FALSE)
  }

  if (length(carpeta_salida) != 1L ||
      is.na(carpeta_salida) ||
      !nzchar(carpeta_salida)) {
    stop("`carpeta_salida` debe ser un nombre válido.", call. = FALSE)
  }

  dir_modelo <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  dir_resultados <- file.path(dir_modelo, carpeta_salida)

  dir.create(
    dir_resultados,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Leer las salidas del modelo
  run <- r4ss::SS_output(dir = dir_modelo)

  if (is.null(run$timeseries) || is.null(run$derived_quants)) {
    stop(
      "La salida de SS_output debe incluir `timeseries` y `derived_quants`.",
      call. = FALSE
    )
  }

  series <- as.data.frame(run$timeseries)
  indicadores <- as.data.frame(run$derived_quants)

  columnas_indicadores <- c("Label", "Value", "StdDev")
  faltan <- setdiff(columnas_indicadores, names(indicadores))

  if (length(faltan) > 0L) {
    stop(
      "`derived_quants` no contiene estas columnas: ",
      paste(faltan, collapse = ", "),
      call. = FALSE
    )
  }

  # Separar las etiquetas con año al final, por ejemplo SSB_2025
  extraido <- stringr::str_match(
    indicadores$Label,
    "^(.*)_((19|20)[0-9]{2})$"
  )

  indicadores$Nombre_indicador <- extraido[, 2]
  indicadores$Anio <- as.integer(extraido[, 3])

  # Indicadores puntuales con IC del 95 %
  z <- stats::qnorm(0.975)

  indicadores_puntuales <- indicadores |>
    dplyr::filter(is.na(Anio)) |>
    dplyr::transmute(
      Indicador = Label,
      Estimacion = Value,
      Desviacion_estandar = StdDev,
      IC95_inferior = Estimacion - z * Desviacion_estandar,
      IC95_superior = Estimacion + z * Desviacion_estandar
    )

  # Series temporales en formato largo
  series_temporales <- indicadores |>
    dplyr::filter(!is.na(Anio)) |>
    dplyr::transmute(
      Anio,
      Indicador = Nombre_indicador,
      Valor = Value,
      Desviacion_estandar = StdDev
    )

  # Series temporales en formato ancho
  series_valores_wide <- series_temporales |>
    dplyr::select(Anio, Indicador, Valor) |>
    tidyr::pivot_wider(
      names_from = Indicador,
      values_from = Valor
    )

  series_sd_wide <- series_temporales |>
    dplyr::select(Anio, Indicador, Desviacion_estandar) |>
    tidyr::pivot_wider(
      names_from = Indicador,
      values_from = Desviacion_estandar,
      names_prefix = "SE_"
    )

  series_temporales_wide <- series_valores_wide |>
    dplyr::left_join(series_sd_wide, by = "Anio") |>
    dplyr::arrange(Anio)

  # Preparar los datos y generar la figura
  datos_figura <- preparar_datos_figura(
    series = series,
    series_temporales_wide = series_temporales_wide,
    area = area,
    temporada = temporada
  )

  figura <- graficar_panel_2x2(
    datos = datos_figura,
    referencia_ssb = referencia_ssb,
    referencia_F = referencia_F
  )

  # Definir rutas de salida
  archivo_puntuales <- file.path(
    dir_resultados,
    "indicadores_puntuales_IC95.csv"
  )

  archivo_series <- file.path(
    dir_resultados,
    "indicadores_series_temporales_columnas.csv"
  )

  archivo_datos_figura <- file.path(
    dir_resultados,
    "datos_figura_2x2.csv"
  )

  archivo_figura <- file.path(
    dir_resultados,
    "biomasa_reclutamiento_mortalidad.png"
  )

  # Guardar archivos
  readr::write_csv(indicadores_puntuales, archivo_puntuales)
  readr::write_csv(series_temporales_wide, archivo_series)
  readr::write_csv(datos_figura, archivo_datos_figura)

  ggplot2::ggsave(
    filename = archivo_figura,
    plot = figura,
    width = 10,
    height = 7,
    units = "in",
    dpi = 300
  )

  invisible(list(
    run = run,
    indicadores_puntuales = indicadores_puntuales,
    series_temporales = series_temporales,
    series_temporales_wide = series_temporales_wide,
    datos_figura = datos_figura,
    figura = figura,
    directorio_salida = dir_resultados,
    archivos = c(
      indicadores_puntuales = archivo_puntuales,
      series_temporales = archivo_series,
      datos_figura = archivo_datos_figura,
      figura = archivo_figura
    )
  ))
}
