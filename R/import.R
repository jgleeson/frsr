#' Import household data
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

#' Import adult data, resulting in a dataset with mostly household variables
#' but one row for every adult. Only imports selected variables.
#' @param folder A folder containing FRS downloaded from UKDS and unzipped
#' @param years A list of years
#' @export

import_adult <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(frs_key, frsyear == year) # import relevant row of key lookup table

    message("Importing survey files for ", year, "...")
    hh <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$household), sep = ""))
    ad <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$adult), sep = ""))

    hh <- hh %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)
    ad <- ad %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)

    # select only the variables we want
    hh <- hh %>% select(any_of(c("sernum", "gvtregn", "country",
                                 "ptentyp2", "hrpnum", "hhagegr3",
                                 "hheth", "hhincbnd")))

    ad <- ad %>% select(any_of(c("sernum", "benunit", "person", "gross4",
                                 "rentprof", "convbl", "empstat",
                                 "ethgr3", "age80", "penben2", "sex",
                                 "relhrp", "marital", "r01", "r02",
                                 "r03", "r04", "r05", "r06", "r07", "r08",
                                 "disacta1", "discora1", "lifesat", "health1",
                                 "disd01", "disd02", "disd03", "disd04",
                                 "disd05", "disd06", "disd07", "disd08",
                                 "disd09", "disd10")))

    hh$frsyear <- year # create year variable

    # join the adult variables onto the household dataset
    hh <- hh %>%
      dplyr::right_join(ad, by = "sernum")

    # add some other variables
    hh <- hh %>%
      mutate(tenure3 = case_when(ptentyp2 == "Owned outright" ~ "Owned",
                                 ptentyp2 == "Owned with mortgage" ~ "Owned",
                                 ptentyp2 == "Rented from Council" ~ "Social rented",
                                 ptentyp2 == "Rented from Housing Association" ~ "Social rented",
                                 ptentyp2 == "Rented privately unfurnished" ~ "Private rented",
                                 ptentyp2 == "Rented privately furnished" ~ "Private rented"))

    hh <- hh %>% mutate(hrpnum = as.integer(hrpnum))

    hh <- hh %>%
      mutate(london = case_when(
        gvtregn == "London" ~ "London",
        gvtregn != "London" & country == "England" ~ "Rest of England",
        country != "England" ~ "Wales, Scotland and NI"
      ))

  })}

#' Import child data, resulting in a dataset with mostly household variables
#' but one row for every child Only imports selected variables.
#' @param folder A folder containing FRS downloaded from UKDS and unzipped
#' @param years A list of years
#' @export

import_child <- function(folder, years){
  files <- list.files(folder, recursive = T)
  hh <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(frs_key, frsyear == year) # import relevant row of key lookup table

    message("Importing survey files for ", year, "...")
    hh <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$household), sep = ""))
    ch <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$child), sep = ""))

    hh <- hh %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)
    ch <- ch %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)

    # select only the variables we want
    hh <- hh %>% select(any_of(c("sernum", "gvtregn", "country",
                                 "ptentyp2", "hrpnum", "hhagegr3",
                                 "hheth", "hhincbnd")))

    ch <- ch %>% select(any_of(c("sernum", "benunit", "person", "gross4",
                                 "ethgr3", "age", "sex", "depend",
                                 "relhrp", "r01", "r02",
                                 "r03", "r04", "r05", "r06", "r07", "r08",
                                 "r09", "r10", "r11", "r12", "r13", "r14",
                                 "disactc1", "discorc1", "chealth1",
                                 "cdisd01", "cdisd02", "cdisd03", "cdisd04",
                                 "cdisd05", "cdisd06", "cdisd07", "cdisd08",
                                 "cdisd09", "cdisd10")))

    hh$frsyear <- year # create year variable

    # join the new variable onto the household dataset
    hh <- hh %>%
      right_join(ch, by = "sernum")

  })}

#' Import benefit unit data
#' @param folder A folder containing FRS downloaded from UKDS and unzipped
#' @param years A list of years
#' @export

import_bu <- function(folder, years){
  files <- list.files(folder, recursive = T)

  bu <- purrr::map_dfr(years, function(year){
    key <- dplyr::filter(frs_key, frsyear == year) # import relevant row of key lookup table

    message("Importing survey files for ", year, "...")
    bu <- haven::read_spss(paste(folder, stringr::str_subset(stringr::str_subset(files, pattern = key$ukda), pattern = key$benunit), sep = ""))

    bu <- bu %>% labelled::to_factor(strict = TRUE) %>% rename_with(tolower)

    bu$frsyear <- year # create year variable

    # take most recent grossing variable, rename it to gross4 if necessary and drop earlier ones
    variable_to_drop <- key$gross_variable_drop[1]
    bu <- bu %>% select(-any_of(variable_to_drop))
    bu <- bu %>% rename(any_of(c("gross4" = "gross3"))) # rename grossing variable if present

    # drop troublesome variables
    bu <- bu %>% select(-any_of(c("mnthcode", "emp", "penage", "sick",
                                  "hrpnum", "hohnum", "hhagegr2",
                                  "hhagegrp", "hhincbnd", "imde", "imdn",
                                  "imds", "imdw", "hbothyr", "gvtregno",
                                  "coatnt1", "coatnt2", "coatnt3", "coatnt4", "coatnt5",
                                  "coatnt6", "coatnt7", "coatnt8", "coatnt9", "coatnt10",
                                  "adbtbl")))

  })}
