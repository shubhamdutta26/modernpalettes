# List of modern color palettes and the order in which they are printed

#' Complete list of palettes.
#'
#' Use names(modern) to return all possible palette names. Current choices are:
#' \code{pgl}
#' @export
modern_palettes <- list(
  pgl = c("#0B0E25", "#0F1D54", "#F57E5D", "#555766", "#E4E3E4")
)

#' modern palette generator
#'
#' These are a handful of color palettes from the PGL2024
#'
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#' \code{pgl}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colors.
#' @return A vector of colors.
#' @export
#' @keywords colors
#' @examples
#' modern_palette("pgl")
#'
#' # If you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- modern_palette(name = "pgl", n = 2, type = "continuous")
modern_palette <-
  function(name, n, type = c("discrete", "continuous")) {
    type <- match.arg(type)

    pal <- modern_palettes[[name]]
    if (is.null(pal))
      stop("Palette not found.")

    if (missing(n)) {
      n <- length(pal)
    }

    if (type == "discrete" && n > length(pal)) {
      stop("Number of requested colors greater than what palette can offer")
    }

    out <- switch(type,
                  continuous = grDevices::colorRampPalette(pal)(n),
                  discrete = pal[1:n])
    structure(out, class = "palette", name = name)
  }
