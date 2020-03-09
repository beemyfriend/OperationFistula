library(tidyverse)
y <- readxl::read_excel('data//STATcompilerExport202038_123430.xlsx')

freqCountries <- y %>%
  dplyr::select(`Country Name`, `Survey Year`) %>%
  dplyr::distinct() %>%
  count(`Country Name`, sort = T) %>%
  dplyr::filter(n >= 3)

test <- y %>%
  dplyr::filter(`Country Name` %in% freqCountries$`Country Name`) %>%
  dplyr::mutate(gender = stringr::str_extract(Indicator, '\\[.*$') %>%
                  stringr::str_remove_all('\\[|\\]'),
                Indicator = stringr::str_remove(Indicator, '\\s\\[.*$') %>%
                  stringr::str_remove('Wife beating justified ')) %>%
  dplyr::select(-`Survey Name`, -`By Variable`, -`CI High`, -`CI Low`) %>%
  dplyr::filter(!is.na(Value),
                `Characteristic Category` != 'Total',
                `Characteristic Category` != 'Total 15-49')

write_csv(test, 'data/YearlyAttitudeTowardsWifeBeating.csv')

#drill in specifically to country
test %>%
  dplyr::filter(`Characteristic Category` == 'Marital status',
                `Country Name` == "Senegal") %>%
  ggplot() +
  geom_line(aes(x = `Survey Year`, y = Value, color = gender)) +
  facet_grid(`Characteristic Label`~Indicator)

diffYear <- test %>%
  dplyr::arrange(`Survey Year`) %>%
  tidyr::nest(data = c('Survey Year', 'Value')) %>%
  dplyr::mutate(data = purrr::map(data, function(x){
    dta <- tibble::tibble(
      first = NA,
      last = NA,
      diff = NA
    )
    if(nrow(x) == 1) return(dta)
    dta$first <- x$Value[1]
    dta$last <- x$Value[nrow(x)]
    dta$diff <- x$Value[nrow(x)] - x$Value[1]
    return(dta)
  })) %>%
  tidyr::unnest() %>%
  dplyr::filter(!is.na(diff)) %>%
  dplyr::mutate(delta = ifelse(diff >= 0, 'positive', 'negative'))

write_csv(diffYear, 'data/DiffYearAttitudeTowardsWifeBeating.csv')

interest <- diffYear %>%
  dplyr::filter(
    Indicator == 'if she burns the food',
    `Characteristic Category` == 'Education',
    `Characteristic Label` == 'No education',
    gender == 'Women'
    ) %>%
  dplyr::arrange(desc(diff))

interestTop <- head(interest, 5)
interestBottom <- tail(interest, 5)

test2 <- rbind(interestTop, interestBottom) %>%
  dplyr::mutate(`Country Name` = factor(`Country Name`, `Country Name`))

ggplot(test2) +
  geom_segment(aes(x = first,
                   xend = last,
                   y = `Country Name`,
                   yend = `Country Name`,
                   color = delta), size = 2, arrow = arrow(length = unit(0.03, "npc")))

