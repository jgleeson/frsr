#' Import Family Resources Survey data
#' @param folder A folder containing FRS downloaded from UKDS and unzipped
#' @param years A list of years
#' @export

import_hh <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(frs_key, frsyear == year) # import relevant row of key lookup table

    message("Importing survey files for ", year, "...")
    hh <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$household), sep = ""))

    hh <- hh %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)

    hh$frsyear <- year # create year variable

    # take most recent grossing variable, rename it to gross4 if necessary and drop earlier ones
    variable_to_drop <- key$gross_variable_drop[1]
    hh <- hh %>% select(-any_of(variable_to_drop))
    hh <- hh %>% rename(any_of(c("gross4" = "gross3"))) # rename grossing variable if present

    # similar thing but for rooms
    variable_to_drop <- key$rooms_variable_drop[1]
    hh <- hh %>% select(-any_of(variable_to_drop))
    hh <- hh %>% rename(any_of(c("rooms10" = "rooms"))) # rename rooms variable if necessary

    # rename tenure variable if necessary
    hh <- hh %>% rename(any_of(c("ptentyp2" = "ptentype")))

    hh <- hh %>% mutate(ptentyp2 = stringr::str_trim(ptentyp2),
                        gvtregn = stringr::str_trim(gvtregn))

    # add some other variables
    hh <- hh %>%
      mutate(tenure3 = case_when(ptentyp2 == "Owned outright" ~ "Owned",
                                 ptentyp2 == "Owned with mortgage" ~ "Owned",
                                 ptentyp2 == "Rented from Council" ~ "Social rented",
                                 ptentyp2 == "Rented from Housing Association" ~ "Social rented",
                                 ptentyp2 == "Rented privately unfurnished" ~ "Private rented",
                                 ptentyp2 == "Rented privately furnished" ~ "Private rented"),
             tenure4 = case_when(ptentyp2 == "Owned outright" ~ "Owned outright",
                                 ptentyp2 == "Owned with mortgage" ~ "Owned with mortgage",
                                 ptentyp2 == "Rented from Council" ~ "Social rented",
                                 ptentyp2 == "Rented from Housing Association" ~ "Social rented",
                                 ptentyp2 == "Rented privately unfurnished" ~ "Private rented",
                                 ptentyp2 == "Rented privately furnished" ~ "Private rented"))

    #  hh <- hh %>% mutate(hrpnum = as.integer(hrpnum))

    # standardise regional labels
    hh <- hh %>%
      mutate(gvtregn = case_when(
        gvtregn == "Yorks and Humberside" ~ "Yorks and the Humber",
        gvtregn == "Yorkshire and Humber" ~ "Yorks and the Humber",
        gvtregn == "Eastern" ~ "East of England",
        gvtregn == "North West and Merseyside" ~ "North West",
        TRUE ~ as.character(gvtregn)
      ))

    # create London variable
    hh <- hh |>
      mutate(london = case_when(
        gvtregn == "London" ~ "London",
        gvtregn != "London" & country == "England" ~ "Rest of England",
        country != "England" ~ "Wales, Scotland and NI"
      ))

    # drop troublesome variables
    hh <- hh %>% select(-any_of(c("mnthcode", "emp", "penage", "sick",
                                  "hrpnum", "hohnum", "hhagegr2", "sewsup",
                                  "hhagegrp", "hhincbnd", "imde", "imdn",
                                  "imds", "imdw", "datyrago", "chrgpd1",
                                  "strmort", "stroths", "strpd2")))
})}
