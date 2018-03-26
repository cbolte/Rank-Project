library(dplyr)
library(ggplot2)


gs_motility <- get_motility(green, species_name = 'Green_Sturgeon')

gs_motility_year <- get_motility(green, species_name = 'Green_Sturgeon', str_startdate = "2014-01-01 00:00:00", str_enddate = "2015-01-01 00:00:00")


motility <- gs_motility_year[order(gs_motility_year$total_rkm, decreasing = TRUE), ]
motility$index <- c(1:nrow(motility))

add_rank <- function(df) {
  det <- df
  det <- det[order(det$total_rkm, decreasing = TRUE), ]
  det$index <- c(1:nrow(motility))
  return(det)
}

ggplot(data = motility, aes(x= index, y= total_rkm, color = (tag))) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = 'none') +
  xlab('Rank') +
  ylab('Motility in River Kilometers') +
  ggtitle('Rank vs Motility of Green Sturgeon in Sacramento/San Joaquin Watershed', subtitle = 'Year 2014') +
  scale_x_discrete(limits = seq(0, nrow(motility), by = 2)) +
  scale_y_discrete(limits = seq(0, (max(motility$total_rkm[1])), by = 250))
  



##############################
#All Year Function with Pipe

start_date <- c("2014-01-01 00:00:00")
end_date <- c("2015-01-01 00:00:00")
year <- c('2014')
df_years <- data.frame(start_date, end_date, year)



ranking_all_years <- function(df, species_name, year_list) {
#Year list should have 3 entries, 'year', 'start', 'end'. This should be made before the loop
  all_years <- 1:nrow(year_list)
  for (y in all_years) { #y should be a number index
    start_datetime <- as.character(year_list$start_date[y])
    end_datetime <- as.character(year_list$end_date[y])
    study_year <- as.character(year_list$year[y])
motility <-  df %>% 
  get_motility(species_name = species_name, str_startdate = start_datetime, str_enddate = end_datetime) %>%
  add_rank
  
p <- ggplot(data = motility, aes(x= index, y= total_rkm, color = (tag))) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = 'none') +
  xlab('Rank') +
  ylab('Motility in River Kilometers') +
  ggtitle(paste('Rank vs Motility of', species_name, 'in Sacramento/San Joaquin Watershed', subtitle = paste('Year', study_year))) + #HERE
  scale_x_discrete(limits = seq(0, nrow(motility), by = 2)) +
  scale_y_discrete(limits = seq(0, (max(motility$total_rkm[1])), by = 250))

plot(p)
  }
}

ranking_all_years(green, 'Green_Sturgeon', df_years)

  
