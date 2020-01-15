#### ---- Libraries ----
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(zoo)
  library(plotly)
  library(tidyverse)
  #library(gridExtra)
  #library(devtools)
  #library(bbplot)

#### ---- Importing Data ----

  EnergyConsumption <- readRDS(file = './Output/EnergyConsumptionClean_3years_kWh.RDS')
  str(EnergyConsumption)
  summary(EnergyConsumption)
  head(EnergyConsumption$Seasons)
  
#### ---- Data Wrangling & Munging ----
  
  #####################################################
  # Date:      26-02-2019                             #
  # Author:    Jeroen Meij                            #
  # File:      Analysis of Smart Home electronic data #
  # Version:   1.0                                    #    
  #####################################################
  
  
  #plot winter vs summer
  ###################################################
  
  
  #make dataset displaying seasonal summary statistics of different sub-meters
  Group_Quarter <- EnergyConsumption %>% group_by(Quarter) %>%
    summarise(Mean_GAP=mean(GAP),
              Mean_Kitchen=mean(Kitchen),
              Mean_Laundry=mean(Laundry),
              Mean_Heater_AC=mean(Heater_AC),
            
    )
  
  
  
  #Gather Data
  Group_Quarter_Gather<- Group_Quarter %>% gather(Submeters, kWh,'Mean_Kitchen', 'Mean_Laundry', 'Mean_Heater_AC', 'Mean_GAP')
  
  #remove spring and fall 
  Group_Quarter_Gather <- Group_Quarter_Gather %>% filter(Quarter == "Summer" | Quarter == "Winter")
  
  #make a barplot displaying Season differences
  ggplot(Group_Quarter_Gather, aes(Submeters, kWh, fill = Quarter)) +
    geom_bar(stat="identity", position ="dodge", color="black") + theme_bw()
    
    
    
##### ----  To check this -----
    #add label providing the percentage change in the kitchen  
    geom_label(aes(x = "Mean_GAP", y = 0.023, label = "-XX%"), 
               hjust = -0.1, 
               vjust = 0,
               lineheight = 0.7,
               colour = "black", 
               fill = "white", 
               label.size = NA, 
               family="Helvetica", 
               size = 5)# +
    
    #add arrow for kitchen
    geom_curve(aes(x = "Kitchen", y = 3,
                   xend = "Kitchen", yend = 1.5), 
               colour = "#555555",
               position = "dodge",
               size = 0.8, 
               curvature = 0,
               arrow = arrow(length = unit(0.03, "npc")) ) +
    
    
    #add label providing the percentage change in the laundry room  
    geom_label(aes(x = "Laundry_room", y = 2.5, label = "-35%"), 
               hjust = -0.1, 
               vjust = 0,
               lineheight = 0.8,
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               family="Helvetica", 
               size = 5) +
    
    
    #add arrow for laundry room  
    geom_curve(aes(x = "Laundry_room", y = 3.5,
                   xend = "Laundry_room", yend = 2), 
               colour = "#555555",
               position = "dodge",
               size = 0.8, 
               curvature = 0,
               arrow = arrow(length = unit(0.03, "npc")) ) +
    
    
    
    #add label providing the percentage change for the heating-airco
    geom_label(aes(x = "Heating", y = 8.2, label = "-35%"), 
               hjust = -0.1, 
               vjust = 0,
               lineheight = 0.8,
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               family="Helvetica", 
               size = 5) +
    
    #add arrow for heating-airco
    geom_curve(aes(x = "Heating", y = 9.2,
                   xend = "Heating", yend = 7.7), 
               colour = "#555555",
               position = "dodge",
               size = 0.8, 
               curvature = 0,
               arrow = arrow(length = unit(0.03, "npc")) ) +
    
    #add style  
    
    theme(legend.position = "right",
          plot.subtitle=element_text(face="italic", color="deepskyblue4", size = 20),
          axis.text.x = element_text(hjust = 1, angle = 25, size = 17)) +
    
    #manually color the bars    
    scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
    
    #add titles  
    labs(title="Winter vs. Summer",
         subtitle = "Differences in average daily kWh per subgroup") +
    
    #scale x/y-axis  
    scale_x_discrete(labels = c("Heating", "Kitchen", "Laundry Room")) + 
    scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 8.5),
                       labels = c("0", "2.5", "5.0", "7.5", "kWh"))
  