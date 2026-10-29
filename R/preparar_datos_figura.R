#' Prepara indicadores de Stock Synthesis para graficar
#'
#' Combina la biomasa total de la salida anual de Stock Synthesis con las
#' series temporales de biomasa desovante, reclutamiento y mortalidad por pesca.
#' Calcula intervalos de confianza normales para estas tres últimas series.
#'
#' @param series Data frame con la salida anual de Stock Synthesis. Debe
#'   contener las columnas `Area`, `Seas`, `Era`, `Yr` y `Bio_all`.
#' @param series_temporales_wide Data frame en formato ancho con las columnas
#'   `Anio`, `SSB`, `SE_SSB`, `Recr`, `SE_Recr`, `F` y `SE_F`.
#' @param area Área que se utilizará. Por defecto, `1`.
#' @param temporada Temporada que se utilizará. Por defecto, `1`.
#' @param conf.level Nivel de confianza para los intervalos. Por defecto, `0.95`.
#' @param escala_biomasa Factor para expresar las biomasas en miles de toneladas.
#'   Por defecto, `1000`.
#' @param escala_reclutamiento Factor para expresar el reclutamiento en millones
#'   de individuos. Por defecto, `1000`.
#'
#' @return Un data frame ordenado por año con biomasa total, biomasa desovante,
#'   reclutamiento, mortalidad por pesca e intervalos de confianza. La biomasa
#'   total no incluye intervalo de confianza.
#'
#' @noRd
preparar_datos_figura <- function(
    series,
    series_temporales_wide,
    area = 1,
    temporada = 1,
    conf.level = 0.95,
    escala_biomasa = 1000,
    escala_reclutamiento = 1000
) {
  # Comprobar que las entradas sean data frames
  if (!is.data.frame(series)) {
    stop("`series` debe ser un data frame.", call. = FALSE)
  }

  if (!is.data.frame(series_temporales_wide)) {
    stop("`series_temporales_wide` debe ser un data frame.", call. = FALSE)
  }

  # Comprobar argumentos escalares
  if (length(area) != 1L || is.na(area)) {
    stop("`area` debe ser un único valor no NA.", call. = FALSE)
  }

  if (length(temporada) != 1L || is.na(temporada)) {
    stop("`temporada` debe ser un único valor no NA.", call. = FALSE)
  }

  if (length(conf.level) != 1L ||
      !is.numeric(conf.level) ||
      is.na(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1) {
    stop("`conf.level` debe ser un número entre 0 y 1.", call. = FALSE)
  }

  if (length(escala_biomasa) != 1L ||
      !is.numeric(escala_biomasa) ||
      is.na(escala_biomasa) ||
      escala_biomasa <= 0) {
    stop("`escala_biomasa` debe ser un número positivo.", call. = FALSE)
  }

  if (length(escala_reclutamiento) != 1L ||
      !is.numeric(escala_reclutamiento) ||
      is.na(escala_reclutamiento) ||
      escala_reclutamiento <= 0) {
    stop(
      "`escala_reclutamiento` debe ser un número positivo.",
      call. = FALSE
    )
  }

  # Comprobar columnas requeridas
  columnas_series <- c("Area", "Seas", "Era", "Yr", "Bio_all")
  columnas_indicadores <- c(
    "Anio", "SSB", "SE_SSB", "Recr", "SE_Recr", "F", "SE_F"
  )

  faltan_series <- setdiff(columnas_series, names(series))
  faltan_indicadores <- setdiff(
    columnas_indicadores,
    names(series_temporales_wide)
  )

  if (length(faltan_series) > 0L) {
    stop(
      "Faltan columnas en `series`: ",
      paste(faltan_series, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(faltan_indicadores) > 0L) {
    stop(
      "Faltan columnas en `series_temporales_wide`: ",
      paste(faltan_indicadores, collapse = ", "),
      call. = FALSE
    )
  }

  # Seleccionar años de la serie temporal para el área y temporada indicadas
  incluir <- !is.na(series$Area) &
    series$Area == area &
    !is.na(series$Seas) &
    series$Seas == temporada &
    !is.na(series$Era) &
    as.character(series$Era) == "TIME"

  incluir[is.na(incluir)] <- FALSE

  serie_biomasa <- series[incluir, , drop = FALSE]

  if (nrow(serie_biomasa) == 0L) {
    stop(
      "No se encontraron registros con el área, temporada y Era especificadas.",
      call. = FALSE
    )
  }

  # Convertir los años a enteros, incluso si vienen como factor
  anios_biomasa <- suppressWarnings(
    as.integer(as.character(serie_biomasa$Yr))
  )
  anios_indicadores <- suppressWarnings(
    as.integer(as.character(series_temporales_wide$Anio))
  )

  if (anyNA(anios_biomasa)) {
    stop("`Yr` contiene años faltantes o no válidos.", call. = FALSE)
  }

  if (anyNA(anios_indicadores)) {
    stop("`Anio` contiene años faltantes o no válidos.", call. = FALSE)
  }

  if (anyDuplicated(anios_biomasa)) {
    stop(
      "Hay más de un registro por año en `series` para el área y temporada seleccionadas.",
      call. = FALSE
    )
  }

  if (anyDuplicated(anios_indicadores)) {
    stop(
      "`series_temporales_wide` debe tener un único registro por año.",
      call. = FALSE
    )
  }

  # Verificar que las variables usadas en los cálculos sean numéricas
  columnas_numericas <- c(
    "Bio_all",
    "SSB", "SE_SSB",
    "Recr", "SE_Recr",
    "F", "SE_F"
  )

  columnas_no_numericas <- columnas_numericas[
    !vapply(
      c(
        list(series$Bio_all),
        lapply(
          series_temporales_wide[
            c("SSB", "SE_SSB", "Recr", "SE_Recr", "F", "SE_F")
          ],
          identity
        )
      ),
      is.numeric,
      logical(1)
    )
  ]

  if (length(columnas_no_numericas) > 0L) {
    stop(
      "Las variables de biomasa, reclutamiento, F y sus desviaciones estándar deben ser numéricas.",
      call. = FALSE
    )
  }

  # Rechazar desviaciones estándar negativas
  desviaciones <- c(
    series_temporales_wide$SE_SSB,
    series_temporales_wide$SE_Recr,
    series_temporales_wide$SE_F
  )

  if (any(desviaciones < 0, na.rm = TRUE)) {
    stop(
      "Las desviaciones estándar no pueden ser negativas.",
      call. = FALSE
    )
  }

  # Biomasa total: no se calcula intervalo de confianza
  biomasa_total <- data.frame(
    Anio = anios_biomasa,
    Biomasa_total = as.numeric(serie_biomasa$Bio_all) / escala_biomasa
  )

  # Calcular el valor crítico para el nivel de confianza solicitado
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  # Indicadores e intervalos de confianza
  indicadores <- data.frame(
    Anio = anios_indicadores,

    Biomasa_desovante =
      series_temporales_wide$SSB / escala_biomasa,
    SSB_inf = pmax(
      0,
      (series_temporales_wide$SSB -
         z * series_temporales_wide$SE_SSB) / escala_biomasa
    ),
    SSB_sup =
      (series_temporales_wide$SSB +
         z * series_temporales_wide$SE_SSB) / escala_biomasa,

    Reclutamiento =
      series_temporales_wide$Recr / escala_reclutamiento,
    Recr_inf = pmax(
      0,
      (series_temporales_wide$Recr -
         z * series_temporales_wide$SE_Recr) / escala_reclutamiento
    ),
    Recr_sup =
      (series_temporales_wide$Recr +
         z * series_temporales_wide$SE_Recr) / escala_reclutamiento,

    F = series_temporales_wide$F,
    F_inf = pmax(
      0,
      series_temporales_wide$F -
        z * series_temporales_wide$SE_F
    ),
    F_sup =
      series_temporales_wide$F +
      z * series_temporales_wide$SE_F
  )

  # Unir las bases por año. Se conservan todos los años de indicadores.
  datos_figura <- merge(
    indicadores,
    biomasa_total,
    by = "Anio",
    all.x = TRUE,
    sort = TRUE
  )

  rownames(datos_figura) <- NULL
  datos_figura
}
