
library(tidyverse)
library(here)


# read in periodic table and create tibble with symbol and mass
periodic_table <- read_csv(here("R", "Periodic-Table-of-Elements.csv"), skip = 2) |>
  select(Symbol, AtomicMass)



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
    stop("Please enter either 'grams' or 'moles'")
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

  .check_chemical_format(compound)
  .check_chemical_format(element)

  perc_mass <- 0
  compound_count <- names(.chemical_elements(compound))
  element_count <- names(.chemical_elements(element))

  if (all(element_count %in% compount_count)) {

    for (elem in element_count) {

      if (element_count[elem] <= compound_count[elem]) {

        percent_mass <- round(compute_mass(element)/compute_mass(compound)*100, 3)

      } else {
        stop("There cannot be more counts of elements in the sub-compound compared to the larger compound")
      }

    }

  } else {
    stop("Please make sure the sub-compound is part of the larger compound")
  }


  print(paste("The percentage mass of", element, "in", compound, "is: ", perc_mass, "%"))
  perc_mass

}





#' Decomposes a chemical to it's elements.
#'
#' @param chemical The molecular formula of the given chemical compound given as a string.
#'
#' @return Named vector of the chemicals elemental components and their counts.
#' @export
#'
#' @examples
.chemical_elements <- function(chemical) {

  primary_list <- c()
  temp_primary_list <- c()
  compound_list <- c()
  simplified_compounds_list <- c()
  raw_element_list <- c()


  # Decompose string into list of components based on capital letters or parenteses
  decompose_elements <- function(string) {

    temp_list <- c()
  }


  # split major components of the given chemical
  primary_list <- decompose_elements(chemical)



  # separate compounds from simple elements



  # simplify the compounds



  # decompose compounds
  for (compound in simplified_compounds_list) {
    temp_list <- decompose_elements(compound)
    temp_primary_list <- c(temp_primary_list, temp_list)
  }


  # merge inital list with decomposed compounds
  primary_list <- c(primary_list, temp_primary_list)


  # break down multiple atoms (e.g. Al2 = Al + Al)


  # count raw_element_list --> named vector (dictionary)
}





#' Check that the chemical formula has correct format
#'
#' @param chemical chemical formula to check
#'
#' @return
#' @export
#'
#' @examples
#' .check_chemical_format("H2O")          ## True
#' .check_chemical_format("NaaaaaaaaaOH") ## False
.check_chemical_format <- function(chemical) {
  allowed_characters <- '[^\(\)A-Za-z0-9]'
  not_allowed_lowercase <- '^[a-z]|\([a-z]'

  if (!(is.character(chemical) & length(chemical) == 1)) {
    stop("Entered value is not a string.")
  }

  # raise 'String contains characters that are not allowed.'
  # raise 'String or subcomponent starts with a lowercase letter.'



}
