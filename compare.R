library(tidyverse)
library(readstata13)

if('all_data.RData' %in% dir()){
  load('all_data.RData')
} else {
  # Get BES data
  source('../maltem_cost_effectiveness/master.R', chdir = TRUE)
  
  # Get opd data
  # source('../hotspots/prepare_data.R', chdir = TRUE)
  load('opd_cleaned.RData')
  
  # Get opd magude data
  opd_mag <- readstata13::read.dta13('survmag_rrs_us.dta')
  # create date
  opd_mag$date <- 
    as.Date(paste0(opd_mag$yr,
                   '-01-01')) +
    (opd_mag$week * 7)
  # Get cases
  opd_mag <-
    opd_mag %>%
    group_by(date) %>%
    summarise(cases = sum(mal_hf))
  
  save(opd,
       df,
       opd_mag,
       file = 'all_data.RData')
}

# Subset opd manhica to get just malaria
opd_man <- opd %>%
  filter(malaria) %>%
  group_by(date) %>%
  summarise(cases = n())

# Get districts in the opds
opd_man <-
  opd_man %>%
  mutate(district = 'MANHICA')
opd_mag <- opd_mag %>%
  mutate(district = 'MAGUDE')

# combine opds
opd <- 
  bind_rows(
    opd_man %>%
      mutate(type = 'OPD'),
    opd_mag %>%
      mutate(type = 'OPD')
  )


# Subset bes to get just manhica and magude
bes <- df %>%
  filter(district %in% c('MAGUDE', 'MANHICA')) %>%
  dplyr::select(date, cases, district) %>%
  group_by(date, district) %>%
  summarise(cases = sum(cases)) %>%
  mutate(type = 'BES')

# Combine all
master <- bind_rows(opd, bes)

# Restrict to just 2012-2016
master <-
  master %>% 
  filter(date >= '2014-01-01')

# Get month
master$date <- as.Date(paste0(format(master$date, '%Y'), 
                              '-',
                              format(master$date, '%m'),
                              '-01'))

# Group by month
master <-
  master %>%
  group_by(date, district, type) %>%
  summarise(cases = sum(cases))


# get percentage of 2012
master <- 
  master %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(district, type) %>%
  mutate(p = cases / mean(cases[year == 2014]) * 100) %>%
  mutate(group = paste0(district, ' ', type)) %>%
  mutate(group = ifelse(group == 'MANHICA OPD',
                        'MANHICA OPD <15 y.o',
                        group))

library(cism)
ggplot(data = master,
       aes(x = date,
           y = p,
           group = group,
           color = group)) +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.3) +
  scale_color_manual(name = 'Source',
                     values = c('darkred',
                                'darkorange',
                                'darkgreen',
                                'green')) +
  facet_wrap(~district, nrow = 2) +
  theme_cism() +
  ylab('Percent of 2012 average') +
  xlab('Date') +
  ggtitle('BES and OPD data',
          'Fellow travelers')
