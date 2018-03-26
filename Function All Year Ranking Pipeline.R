start_date <- c("2006-01-01 00:00:00","2007-01-01 00:00:00", "2008-01-01 00:00:00", "2009-01-01 00:00:00",
"2010-01-01 00:00:00", "2011-01-01 00:00:00", "2012-01-01 00:00:00", "2013-01-01 00:00:00", 
"2014-01-01 00:00:00", "2015-01-01 00:00:00", "2016-01-01 00:00:00", "2017-01-01 00:00:00")

end_date <- c("2007-01-01 00:00:00", "2008-01-01 00:00:00", "2009-01-01 00:00:00",
              "2010-01-01 00:00:00", "2011-01-01 00:00:00", "2012-01-01 00:00:00", "2013-01-01 00:00:00", 
              "2014-01-01 00:00:00", "2015-01-01 00:00:00", "2016-01-01 00:00:00", "2017-01-01 00:00:00",
              "2018-01-01 00:00:00")

length(year)

year <- c('2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017')
df_years <- data.frame(start_date, end_date, year)

ranking_all_years(green, 'Green_Sturgeon', df_years, 'All Green Sturgeon Motilities from 2006 - 2017')


ranking_all_years <- function(df, species_name, year_list, pdf_name) {
  #Year list should have 3 entries, 'year', 'start', 'end'. This should be made before the loop
  pdf(paste(pdf_name, ".pdf", sep = "" )) 
  all_years <- 1:nrow(year_list)
  for (y in all_years) { #y should be a number index
    start_datetime <- as.character(year_list$start_date[y])
    end_datetime <- as.character(year_list$end_date[y])
    study_year <- as.character(year_list$year[y])
    print(start_datetime)
    print(end_datetime)
    print(study_year)
    motility <-  df %>% 
      get_motility(species_name = species_name, str_startdate = start_datetime, str_enddate = end_datetime) %>%
      add_rank
    
    p <- ggplot(data = motility, aes(x= index, y= total_rkm, color = (tag))) + 
      geom_point() + 
      geom_line() +
      theme(legend.position = 'none') +
      xlab('Rank') +
      ylab('Motility in River Kilometers') +
      ggtitle(paste(study_year, 'Rank vs Motility of', species_name, 'in Sacramento/San Joaquin Watershed')) + #HERE
      scale_x_discrete(limits = seq(0, nrow(motility), by = 2)) +
      scale_y_discrete(limits = seq(0, 4500), by = 250)
    
    plot(p)
  }
  dev.off()
}

