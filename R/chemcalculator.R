#' Computes the molar mass of the given chemical compound.
#'
#' @param chemical The molecular formula given as a string.
#'
#' @return The molar mass of the given formula in g/mol.
#' @export
#'
#' @examples
#' compute_mass("H2O") ## returns 18.01528
#'
#' compute_mass("C12H22O11") ## returns 342.3
#'
#' compute_mass("Al2(SO4)3") ## returns 342.15
#'
#' compute_mass("(NH4)HS") ## returns 51.107
convert_mass <- function(chemical) {

}


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
  grams_per_mole <- compute_mass(formula)
  if(convert_to == "grams") {
    results <- round(mass / grams_per_mole, 3)
  } else if (convert_to == "moles") {
    results <- round(mass * grams_per_mole, 3)
  } else {
    stop("Please enter either 'grams" or 'moles'")
  }
  return(results)
}


#' Calculates the percentage mass of an element (or compound) in a compound
#'
#' @param compound the chemical formula of the full compound
#' @param element the chemical formula of the element or compound of interest
#'
#' @return the percentage mass of the element or compound of interest
#' @export
#'
#' @examples
#' percent_mass("H2O", "O") ## returns 88.79
#' percent_mass("H2O", "H") ## returns 11.19
#' percent_mass("NaOH", "OH") ## returns 42.52
percent_mass <- function(compound, element) {

}
