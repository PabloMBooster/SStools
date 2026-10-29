#' Grafica indicadores de Stock Synthesis en un panel 2 x 2
#'
#' Genera gráficos de biomasa total, biomasa desovante, reclutamiento y
#' mortalidad por pesca. Añade intervalos de confianza a los tres últimos
#' indicadores y presenta cada gráfico con borde.
#'
#' @param datos Data frame preparado por `preparar_datos_figura()`. Debe
#'   contener `Anio`, `Biomasa_total`, `Biomasa_desovante`, `SSB_inf`,
#'   `SSB_sup`, `Reclutamiento`, `Recr_inf`, `Recr_sup`, `F`, `F_inf` y `F_sup`.
#' @param referencia_ssb Valor opcional para la línea horizontal de referencia
#'   de biomasa desovante, en miles de toneladas. Por defecto, `NULL`.
#' @param referencia_F Valor opcional para la línea horizontal de referencia
#'   de mortalidad por pesca, en año⁻¹. Por defecto, `1`.
#'
#' @return Un objeto de clase `patchwork` con los cuatro gráficos.
#'
#' @details
#' La biomasa total no incluye intervalo de confianza. Los intervalos de las
#' otras series deben estar calculados previamente en `datos`.
#'
#' @noRd
graficar_panel_2x2 <- function(
    datos,
    referencia_ssb = NULL,
    referencia_F = 1
) {
  # Validar la base
  if (!is.data.frame(datos)) {
    stop("`datos` debe ser un data frame.", call. = FALSE)
  }

  columnas_requeridas <- c(
    "Anio",
    "Biomasa_total",
    "Biomasa_desovante",
    "SSB_inf",
    "SSB_sup",
    "Reclutamiento",
    "Recr_inf",
    "Recr_sup",
    "F",
    "F_inf",
    "F_sup"
  )

  columnas_faltantes <- setdiff(columnas_requeridas, names(datos))

  if (length(columnas_faltantes) > 0L) {
    stop(
      "Faltan columnas en `datos`: ",
      paste(columnas_faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  # Validar las referencias opcionales
  if (!is.null(referencia_ssb) &&
      (!is.numeric(referencia_ssb) ||
       length(referencia_ssb) != 1L ||
       is.na(referencia_ssb))) {
    stop(
      "`referencia_ssb` debe ser NULL o un único valor numérico.",
      call. = FALSE
    )
  }

  if (!is.null(referencia_F) &&
      (!is.numeric(referencia_F) ||
       length(referencia_F) != 1L ||
       is.na(referencia_F))) {
    stop(
      "`referencia_F` debe ser NULL o un único valor numérico.",
      call. = FALSE
    )
  }

  # Tema común para los cuatro paneles
  tema_figura <- ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "plain"),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(color = "black"),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      )
    )

  # a) Biomasa total, sin intervalo de confianza y con eje desde cero
  p_biomasa_total <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = Anio, y = Biomasa_total)
  ) +
    ggplot2::geom_line(
      color = "black",
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::labs(
      title = "a) Biomasa total",
      x = NULL,
      y = "Biomasa total (miles de t)"
    ) +
    tema_figura

  # b) Biomasa desovante, con IC del 95 %
  p_ssb <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = Anio, y = Biomasa_desovante)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = SSB_inf, ymax = SSB_sup),
      fill = "skyblue2",
      alpha = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      color = "black",
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::labs(
      title = "b) Biomasa desovante",
      x = NULL,
      y = "Biomasa desovante (miles de t)"
    ) +
    tema_figura

  if (!is.null(referencia_ssb)) {
    p_ssb <- p_ssb +
      ggplot2::geom_hline(
        yintercept = referencia_ssb,
        color = "blue",
        linewidth = 0.5
      )
  }

  # c) Reclutamiento, con IC del 95 %
  p_reclutamiento <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = Anio, y = Reclutamiento)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Recr_inf, ymax = Recr_sup),
      width = 0,
      color = "gray40",
      linewidth = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      color = "black",
      size = 1.5,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::labs(
      title = "c) Reclutamiento (edad 0)",
      x = NULL,
      y = "Reclutamiento (millones de individuos)"
    ) +
    tema_figura

  # d) Mortalidad por pesca, con IC del 95 %
  p_f <- ggplot2::ggplot(
    datos,
    ggplot2::aes(x = Anio, y = F)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = F_inf, ymax = F_sup),
      fill = "skyblue2",
      alpha = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      color = "black",
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::labs(
      title = "d) Mortalidad por pesca",
      x = NULL,
      y = expression(F~(año^{-1}))
    ) +
    tema_figura

  if (!is.null(referencia_F)) {
    p_f <- p_f +
      ggplot2::geom_hline(
        yintercept = referencia_F,
        color = "blue",
        linewidth = 0.5
      )
  }

  # Ensamblar los cuatro gráficos en dos columnas
  patchwork::wrap_plots(
    p_biomasa_total,
    p_ssb,
    p_reclutamiento,
    p_f,
    ncol = 2
  )
}
