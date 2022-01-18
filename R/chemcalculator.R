#' Converts moles to grams or grams to moles
#'
#' @param formula A checmical formula for the conversion
#' @param mass The mass of molecule that needs to be converted (grams or moles)
#' @param convert_to The type of conversion to be made to either "moles" or "grams"
#'
#' @return A number mass that is converted to either moles or grams
#' @export
#'
#' @examples
#' moles_grams_converter("H2O", 0.05555, "moles") ## returns 1.0007
#'
#' moles_grams_converter("H2O", 18.01528, "grams") ## returns 1.000
#'
moles_grams_converter <- function(formula, mass, convert_to) {

}
