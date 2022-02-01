library(readr)
library(here)


path = here("inst/extdata", "Periodic-Table-of-Elements.csv")

# read in periodic table and create tibble with symbol and mass
periodic_table <- read_csv(path, skip = 2) |>
  select(Symbol, AtomicMass)

#' Computes the molar mass of the given chemical compound.
#'
#' @param chemical The molecular formula given as a string.
#'
#' @return The molar mass of the given formula in g/mol.
#' @importFrom rje is.subset
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
compute_mass <- function(chemical) {
  .check_chemical_format(chemical)

  raw_elements = .chemical_elements(chemical)


  if (is.subset(raw_elements$elements, periodic_table$Symbol)){
    # pass
  }
  else {
    stop('Chemical compound contains element not in periodic table.')
  }

  # map atomic masses from periodic table and multiply by atomic counts
  raw_elements <- left_join(raw_elements, periodic_table,
                            by = c("elements" = "Symbol"))

  raw_elements <- raw_elements |>
    mutate(mass = Freq * AtomicMass)

  # sum and return mass
  sum(raw_elements$mass)
}


#' Converts moles to grams or grams to moles
#'
#' @param formula A checmical formula for the conversion
#' @param mass The mass of molecule that needs to be converted (grams or moles)
#' @param convert_from The type of conversion to be made from either "moles" or "grams"
#'
#' @return A number mass that is converted to either moles or grams
#' @export
#'
#' @examples
#' moles_grams_converter("H2O", 0.05555, "moles") ## returns 1.0007
#'
#' moles_grams_converter("H2O", 18.01528, "grams") ## returns 1.000
moles_grams_converter <- function(formula, mass, convert_from) {
  grams_per_mole <- compute_mass(formula)
  if(convert_from == "grams") {
    results <- round(mass / grams_per_mole, 3)
  } else if (convert_from == "moles") {
    results <- round(mass * grams_per_mole, 3)
  } else {
    stop("Please enter either 'grams' or 'moles'")
  }
  results
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
#'
#' percent_mass("H2O", "O") ## returns 88.79
#' percent_mass("H2O", "H") ## returns 11.19
#' percent_mass("NaOH", "OH") ## returns 42.52
#'
percent_mass <- function(compound, element) {
#
  .check_chemical_format(compound)
  .check_chemical_format(element)

  perc_mass <- 0
  compound_count <- .chemical_elements(compound)
  element_count <- .chemical_elements(element)

  # get number of rows where an element in element has greater number than in compound
  elem_comp_compare <- inner_join(compound_count, element_count, by = "elements") |>
    rename(comp_freq = Freq.x, elem_freq = Freq.y) |>
    mutate(diff = elem_freq - comp_freq) |>
    filter(diff > 0) |>
    nrow()

  if (all(element_count$elements %in% compound_count$elements)) {

    if (elem_comp_compare == 0) {

      perc_mass <- round(compute_mass(element)/compute_mass(compound)*100, 3)

    } else {
      stop("There cannot be more counts of elements in the sub-compound compared to the larger compound")
    }



  } else {
    stop("Please make sure the sub-compound is part of the larger compound")
  }


  print(paste("The percentage mass of", element, "in", compound, "is:", perc_mass, "%"))
  perc_mass

}



#' Decomposes a chemical to it's elements.
#'
#' @param chemical The molecular formula of the given chemical compound given as a string.
#'
#' @return Dataframe of the chemicals elemental components and their counts.
#' @import dplyr stringr
#' @export
#'
.chemical_elements <- function(chemical) {

  primary_list <- c()
  temp_primary_list <- c()
  compound_list <- c()
  simplified_compounds_list <- c()
  raw_element_list <- c()

  # Decompose string into list of components based on capital letters or parenthesis
  decompose_elements <- function(string) {
    decomp_regex <- '(\\(.*?\\)\\d+)|(\\(.*?\\))|([A-Z][^A-Z|(]*)'
    has_compound <- string |> str_extract_all(decomp_regex)
    has_compound[[1]]
  }

  # split major components of the given chemical
  primary_list <- decompose_elements(chemical)

  # separate compounds from simple elements
  compound_regex = '\\(.*?\\)\\d+|\\(.*?\\)'
  compound_list <- primary_list |> str_extract_all(compound_regex)
  compound_list <- Filter(length, compound_list)
  if (length(compound_list) > 0) {
    compound_list <- compound_list[[1]]
    primary_list <- primary_list[!primary_list %in% compound_list]
  }

  # simplify the compounds
  simp_compound_regex = '\\)\\d+'

  for (compound in compound_list) {
    temp_list <- c()

    trim <- compound |> str_match(simp_compound_regex)
    trim <- trim[[1]]

    # special case when no number after bracket
    if (is.na(trim)) {
      trim <- ")"
    }

    if (nchar(trim) > 0) {
      length = as.integer(nchar(trim))
      units = as.integer(substr(trim, 2, length))
      simplified_compound = substr(compound, 2, nchar(compound) - length)
    }
    else {
      length = 1
      units = 1
      simplified_compound = substr(compound, 2, length)
    }

    temp_list <- rep(simplified_compound, each = units)

    simplified_compounds_list <- c(simplified_compounds_list, temp_list)
  }

  # decompose compounds
  for (compound in simplified_compounds_list) {
    temp_list <- decompose_elements(compound)
    temp_primary_list <- c(temp_primary_list, temp_list)
  }

  # merge inital list with decomposed compounds
  primary_list <- c(primary_list, temp_primary_list)

  # break down multiple atoms (e.g. Al2 = Al + Al)
  raw_element_regex = '\\d+'

  for (element in primary_list) {
    temp_list <- c()

    trim <- element |> str_match(raw_element_regex)

    if (!is.na(trim)) {
      length <- as.integer(nchar(trim))
      units <- as.integer(substr(trim, 1, length))
      raw_element <- substr(element, 1, nchar(element) - length)
      temp_list <- rep(raw_element, each = units)
    }
    else {
      temp_list <- element
    }

    raw_element_list <- c(raw_element_list, temp_list)
  }

  as.data.frame(table(raw_element_list)) |>
    rename(elements = raw_element_list)
}

#' Check that the chemical formula has correct format
#'
#' @param chemical chemical formula to check
#'
#' @return True or FALSE for if chemical formula has the correct format
#' @export
#'
#' @examples
#' .check_chemical_format("H2O")          ## True
#' .check_chemical_format("NaaaaaaaaaOH") ## False
.check_chemical_format <- function(chemical) {
  allowed_characters <- '[^\\(\\)A-Za-z0-9]'
  not_allowed_lowercase <- '^[a-z]|\\([a-z]'

  if (!(is.character(chemical) & length(chemical) == 1)) {
    stop("Entered value is not a string.")
  }

  if (!str_detect(chemical, allowed_characters, negate = TRUE)) {
    stop('String contains characters that are not allowed.')
  }

  if (str_detect(chemical, not_allowed_lowercase)) {
    stop('String or subcomponent starts with a lowercase letter.')
  }
}

