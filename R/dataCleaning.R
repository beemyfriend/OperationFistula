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

test %>%
  dplyr::filter(`Characteristic Category` == 'Marital status',
                `Country Name` == "Senegal") %>%
  ggplot() +
  geom_line(aes(x = `Survey Year`, y = Value, color = gender)) +
  facet_grid(`Characteristic Label`~Indicator)
