library(tidyverse)
library(stringr)
library(patchwork)
library(shiny)



data <- read.csv('data/covidvaccine.csv')

anti_vax_tags <- c('plandemic', 'scamdemic', 'wakeup', 'covid1984', 'covidhoax', 'nomasks', '#nomask', 'COVIDVaccineInjuries')


filtered <- data %>%
  mutate(anti_vax = ifelse(grepl(anti_vax_tags[1], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[2], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[3], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[4], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[5], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[6], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[6], hashtags, ignore.case = TRUE) |
                             grepl(anti_vax_tags[7], hashtags, ignore.case = TRUE), 
                           1, 0))


check_tag = c('autism', 'eugenics', 'PuppetState', 'nwo', 'Great Reset', 'wakeup')


shear_age <- filtered[8113:nrow(filtered),]


filter_age <- shear_age %>% 
  mutate(format_date = as.Date(date, format= "%Y-%m-%d")) %>% 
  mutate(format_created = as.Date(user_created, format= "%Y-%m-%d")) %>% 
  mutate(acc_age = as.numeric(difftime(as.Date(format_date), as.Date(format_created), units = 'days'))) %>% 
  select(acc_age, anti_vax)


specify_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall=k))
  
} 





server <- function(input, output) {
  output$age_plot <- renderPlot({
    
    
    
    
    
    ggplot(filter_age, aes(y = acc_age, x = as.factor(anti_vax), color = as.factor(anti_vax))) +
      geom_boxplot() + 
      stat_summary(fun = "quantile",
                   geom = "text",
                   aes(label = trimws(format(round(as.numeric(sprintf("%1.1f", ..y..)), 0), nsmall = 0))),
                   position = position_nudge(x = 0.2, y = 125),
                   color = "black") +
      scale_y_discrete(name = "Account Age (Days)", labels = null, expand = expansion(mult = c(0, 0.02))) +
      scale_color_manual(name = "", values = c("black", "red"), labels = c("All #CovidVaccine Tweets", "Anti-vax Tweets")) + 
      scale_x_discrete(name = NULL, labels = NULL)
    
  })
  
  
  
  # 
  # num_tag = 0
  # num_without = 0
  # 
  # for (i in 1:nrow(filtered_tags)) {
  #   if(grepl(check_tag[1], filtered_tags[i,], ignore.case = TRUE) == TRUE | 
  #      grepl(check_tag[2], filtered_tags[i,], ignore.case = TRUE) == TRUE | 
  #      grepl(check_tag[3], filtered_tags[i,], ignore.case = TRUE) == TRUE | 
  #      grepl(check_tag[4], filtered_tags[i,], ignore.case = TRUE) == TRUE | 
  #      grepl(check_tag[5], filtered_tags[i,], ignore.case = TRUE) == TRUE) {
  #     #print('Contains ' + check_tag)
  #     print(filtered_tags[i,])
  #     num_tag = num_tag + 1
  #     
  #   } else{
  #     num_without = num_without + 1
  #     
  #   }
  # }
  
  conspiracy_data <- shear_age %>% 
    mutate(conspiracy = ifelse(grepl(check_tag[1], hashtags, ignore.case = TRUE) | 
         grepl(check_tag[2], hashtags, ignore.case = TRUE) |
         grepl(check_tag[3], hashtags, ignore.case = TRUE) |
         grepl(check_tag[4], hashtags, ignore.case = TRUE) |
         grepl(check_tag[5], hashtags, ignore.case = TRUE), 1, 0))
  
  cons_group <- conspiracy_data %>% 
    group_by(anti_vax) %>% 
    summarise(mean = mean(conspiracy))
  cons_group


}