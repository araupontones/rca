library(tidyverse) ##for cleaning, transforming and visualizing
library(knitr) ## for displaying tables
library(CoordinateCleaner) ##for countries' centroids
library(leaflet) ## for mapping
#library(htmltools)
#library(webshot)
#library(htmlwidgets)
#library(mapview)
#library(kableExtra)
library(treemapify) ## for treemaps
library(extrafont) ##for fonts in charts
library(ggrepel) ##for labels
#loadfonts(device = "win")
library(ggalt) ##for dumbbell chart
library(scales) ##for labelling axis


 
options("scipen"=100, "digits"=4)  ##to print large numbers 

delivery_dir = "2.Data/3.Delivery"



##------------------------------------------------read data ----------------------------------------------------------

##centroids
centroids = CoordinateCleaner::countryref %>%
  filter(type == "country") %>%
  select(name, centroid.lon, centroid.lat) %>%
  rename(lon = centroid.lon,
         lat = centroid.lat,
         country = name) %>%
  group_by(country) %>%
  slice(1) %>%
  mutate(country = if_else(country == "Myanmar (Burma)", "Myanmar", country)) %>%
  ungroup()

##main data, joint with centroids
mainqn = read.csv("2.Data/1.raw/rca.csv") %>%
  mutate(country = if_else(country=="Korea", "South Korea", country)) %>%
  left_join(centroids)




##internal data 

internal = read.csv("2.Data/1.raw/internal.csv") %>%
  filter(Country != "TOTAL ALL COUNTRIES") %>%
  select(Question, Country, TOTAL.2000.to.2019) %>% ##keep relevant variables
  rename(Total =  TOTAL.2000.to.2019) 
  
##fill empty rows with question name
internal$Question[internal$Question=='']<-NA

##reshape 
internal2 = internal %>%
  fill(Question, .direction = 'down') %>%
  spread(Question, Total) %>%
    rename(country = Country,
           traings_conducted = `How many training courses in mutation breeding  were conducted?` ,
           people_trained = `How many people were trained in regional training courses in mutation breeding?`,
           women_trained = `How many of these trainees were women?`,
           expert_missions = `How many expert missions to this country occurred?`,
           expert_attendees = `How many experts from this country attended expert missions to other countries?`,
           expert_women = `How many of the expert from this country were women?`,
           workshops = `How many meetings/workshops for senior members of mutation breeding research teams were facilitated?`,
           worskshop_participants = `How many senior members of mutation breeding research teams participated in these type of meetings/workshops?`) %>%
  mutate(country = if_else(country=="Korea", "South Korea", country)) %>%
  filter(country != "Vienna, AUSTRIA") %>%
  left_join(centroids)


t = mainqn %>%
  select(country, institutions, funders, publications, publications_sci)

names(mainqn)
## crops 

##main data, joint with centroids
cropsqn = read.csv("2.Data/1.raw/crops.csv") %>%
  mutate(country = if_else(country=="Korea", "South Korea", country)) %>%
  left_join(centroids)


## QA tables

 QA_pesticide = cropsqn %>%
   select(country, crop, reduction_pesticide) %>%
   filter(!is.na(reduction_pesticide)) %>%
   mutate(reduction_pesticide = percent(reduction_pesticide))
 
 QA_fertilizer = cropsqn %>%
   select(country, crop, reduction_fertilizer) %>%
   filter(!is.na(reduction_fertilizer)) %>%
   mutate(reduction_fertilizer = percent(reduction_fertilizer))
 
 QA_water = cropsqn %>%
   select(country, crop, increase_water) %>%
   filter(!is.na(increase_water)) %>%
   mutate(increase_water = percent(increase_water))
 
 QA_soil = cropsqn %>%
   select(country, crop, increase_soil) %>%
   filter(!is.na(increase_soil)) %>%
   mutate(increase_water = percent(increase_soil)) %>%
   filter(crop =="Soybean")
 
 QA_institutions = mainqn %>%
   select(country, institutions)
 
 


### ------------------------------------------- Clean ----------------------------------------------------------------

## Bangladesh
 
 mainqn$institutions[mainqn$country=="Bangladesh"] <-10 ## Confrimed that the answer is 10, not 150

##Korea
cropsqn$yield_control[cropsqn$country=="South Korea" & cropsqn$crop=="Bean"]<- 2 ##clean typo in yield control of Bean (it was 100)
cropsqn$yield_control[cropsqn$country=="South Korea" & cropsqn$crop=="Rice"]<- 4.75 ##clean typo in yield control of Rice (it was 105), Korea did not repsond correctly
cropsqn$yield[cropsqn$country=="South Korea" & cropsqn$crop=="Rice"]<- 5 ##clean typo in yield control of Rice (it was 105)


###Japan
cropsqn$yield_control[cropsqn$country=="Japan" & cropsqn$crop=="Soybean"]<- 1.7 ##clean typo in yield control of Rice (it was 105), Korea did not repsond correctly
cropsqn$env_trait_Increase.of.water.efficiency[cropsqn$country=="Japan"] ="No" ## confirmed that original anser was an error
cropsqn$increase_water[cropsqn$country=="Japan"] =NA
cropsqn$env_trait_Increase.soil.fertility[cropsqn$country=="Japan"] ="No" ## confirmed that original anser was an error
cropsqn$increase_soil[cropsqn$country=="Japan"] =NA


###Pakistan
cropsqn$area[cropsqn$country=="Pakistan" & cropsqn$crop == "Rice"] <- 6000000 ## After clarifying with Pakistan's expert
cropsqn$area[cropsqn$country=="Pakistan" & cropsqn$crop == "Mungbean"] <- 2280000 ## After clarifying with Pakistan's expert







## save to delivery folder

write.csv(cropsqn, file.path(delivery_dir, "crops.csv"))
write.csv(mainqn, file.path(delivery_dir, "rca.csv"))


### -----------------------------------------------Define style ----------------------------------------------------------------------------

##colors 

blue_IAEA = '#3561CE'
blue_light = '#5091CD'
gray = '#b2b2b2'
gray_dark ='#6D6E70'
light_gray = '#ECECEC'
color_text = 'black'
yellow_IAEA = '#F0AD4E'
second_color = '#F0AD4E'
verde = '#6DBD44'
verde_light= '#C3E0AE'
morado = '#7D7FB7'
morado_ligth = '#ACAED4'
orange = 'F7981D'

##Font 


font_charts = "Segoe UI Historic"
  
# "MS Reference Sans Serif"
# "Segoe UI Historic"
# "Verdana"
  




##Caption 

caption_online = "Source: IAEA's online survey, 2020"
caption_internal = "Source: IAEA's internal data, 2020"

##Chart theme 

tema  = theme(
      ##Font
      text = element_text(font_charts),
      ##Bakcground
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = alpha(yellow_IAEA,.3)),
      ##Axis
      axis.ticks = element_blank(),
      axis.text = element_text(hjust = 0, size = 12, face = "bold", colour = color_text),
      axis.title = element_text(size = 12, face = 'bold', colour = color_text),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      ##Title
      plot.title = element_text(hjust = 0, size = 18, colour = color_text, face = 'bold'),
      
      ##legend
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12)
)


##vector with number names 
number_names = c("one", "two", "three", "four", "five", "six", "seven", "eitght", "nine")

## function to convert number into name (for report correct format)
to_number =  function(x){
  if(x < 10){
    y = number_names[x]
  } else{
    y = x  
    
  }
  
  return(y)
  
}

##------------------------------- Map -----------------------------------------------------------

### Map ---------------------------------------
map = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers(data = internal2,
                   lat =  ~lat,
                   lng = ~lon,
                   radius = 5,
                   color = blue_IAEA,
                   #stroke = F,
                   fillOpacity = .9,
                   label = ~country)




## ------------------------------Countries-------------------------------------------------------

## name of all RCA countries
countries = sort(unique(internal2$country))
countries_sentence = combine_words(countries)

##Name of countries that did not participate in the online survey

countries_not = combine_words(setdiff(countries,unique(mainqn$country)))


### ------------------------------------Criterion 1: Increased food production---------------------------------------------------------


##Number of mutatant varieties 

 lines_developed= prettyNum(sum(cropsqn$lines_developed), big.mark = ",")
  varieties_developed = prettyNum(sum(cropsqn$varieties_developed), big.mark = ",")
  
  ##table of countries by development status 
  
  table_developed = cropsqn %>%
    group_by(country) %>%
    summarise(`Lines developed` = sum(lines_developed),
              `Varieties developed` = sum(varieties_developed)) %>%
    mutate(`Has developed lines` = if_else(`Lines developed`>0, "Yes", "No"),
           `Has developed varieties` = if_else(`Varieties developed`>0, "Yes", "No")) %>%
    select(country, `Has developed lines`,`Lines developed`, `Has developed varieties`, `Varieties developed` )
  
  havent_developedVarities = knitr::combine_words(table_developed[table_developed$`Has developed varieties`=="No", ]$country)
  
  havent_lines_perc = paste0(round(mean(table_developed$`Has developed lines`=="No") *100, digits = 0), "%")
  havent_varieties_perc = paste0(round(mean(table_developed$`Has developed varieties`=="No") *100, digits = 0), "%")

  havent_lines_perc
  havent_varieties_perc
  
  ##table for annex

  
  table_crops_annex = cropsqn %>%
    select(country, crop, lines_developed, varieties_developed, area, yield, yield_control) %>%
    mutate(area = prettyNum(area/1000,big.mark = ",")) %>%
    rename(`Lines developed` = lines_developed,
           `Varities developed` = varieties_developed,
           Crop = crop,
           Country = country,
           `Cumulative Growing area (in thousand ha)` = area,
           `Yield (tonnes/ha)` = yield,
           `Yield Control (tonnes/ha)` = yield_control) %>%
    mutate(`Lines developed` = prettyNum(`Lines developed`, big.mark = ",")) %>%
    arrange(Country, Crop)
  
  
## varieties developed by crop
  
  
  data_crops = cropsqn %>%
    group_by(crop) %>%
    summarise(varieties = sum(varieties_developed),
              lines = sum(lines_developed),
              yield = mean(yield, na.rm = T),
              yield_control = mean(yield_control, na.rm = T),
              yield_diff = round(yield - yield_control, digits = 2),
              yield_perc = (yield-yield_control)/yield_control *100,
              area = sum(area, na.rm = T)/1000,
              production = area * yield
              )%>% ##weight factor
    ungroup() %>%
    mutate(weight = round(production/sum(production,na.rm = T), digits = 3))
  
 
  
  yield_average = paste0(round(mean(data_crops$yield_perc, na.rm = T), digits = 1),"%")
  
 ##chart lines vs varieties developed 
  chart_develop = ggplot(data_crops %>%
           filter(lines>15),
         aes(x=lines,
             y = varieties,
             label=crop))+
    geom_point(size = 3,
               color = second_color,
               fill = blue_IAEA,
               shape = 21) +
    geom_label_repel(aes(label = crop),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = blue_light,
                     color = yellow_IAEA) +
    ylab("Mutant varieties developed")+
    xlab("Mutant lines developed") +
    labs(#title = "Mutant lines and varities developed under RCA since 2000.\nBy crop",
         caption = caption_online)+
    tema
  
  
##mutant varieties developed
    chart_crops =ggplot(data = data_crops %>%
                filter(varieties>0), 
    
    aes(x=reorder(crop, varieties),
                     y = varieties)
    ) +
        geom_text(aes(label= varieties),
                  nudge_y = 5,
                  size = 5,
                  color = second_color) +
      geom_bar(stat = 'identity',
               fill = blue_IAEA) +
      coord_flip() +
      ylab("Number of mutant varieties developed") +
      xlab("")+
      labs(#title = "Total of mutant varieties developed under RCA since 2000.\nBy crop",
           caption = caption_online) +
        scale_y_continuous(limits = c(0,130),
                           breaks = seq(0,130,15)) +
      tema
    
      seq(0,130,10)
  
##yield Control vs mutant
    
    data_chart_yield = data_crops %>%
      filter(!is.nan(yield)) %>%
      select(crop, yield_control, yield) %>%
      gather(status, value, -crop) %>%
      filter(!crop %in% c("Tomato", "Banana")) %>%
      arrange(value) %>%
      mutate(crop = factor(crop,
                           levels = unique(.$crop)))
    
    
    
    
    data_dumbell=data_crops %>%
      filter(!crop %in% c("Tomato", "Banana"),
             !is.nan(yield)) %>%
      arrange(yield) %>%
      mutate(crop = factor(crop,
                           levels = unique(.$crop)))
    
    chart_productivity = ggplot(data_chart_yield,
           aes(x=value,
               y = crop
               )
           )+
      geom_dumbbell(data=data_dumbell,
                    aes(x=yield,
                        xend= yield_control,
                        y = crop),
                    color =gray,
                    size= 1.2)+
      geom_point(size = 3,
                 aes(color = status)) +
      
      geom_text(data=data_dumbell,
                aes(label = paste("+",yield_diff),
                    y = crop,
                    x = 7.5),
                size = 4,
                fontface = 'bold',
                hjust = .5,
                color = gray)+
      
      
      geom_text(aes(x=8,
                    y = 12,
                    label= "Change in\n(tonnes/ha)"
                    ),
                size=5,
                hjust = 1,
                vjust = 1,
                color = gray)+
      
      scale_color_manual(values = c(blue_IAEA, second_color),
                         labels = c("Mutant", "Control")) +
      xlab('Yield productivity (tonnes/ha)')+
      ylab("") +
      xlim(c(1,10)) +
      labs(#title = "Average change in yield productivity between mutant\nand control crops",
           caption = caption_online)+
      tema +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.key = element_rect(fill = "white"),
            plot.background = element_blank(),
            panel.grid.major = element_line(colour = alpha(yellow_IAEA,.15))
            ) +
      scale_x_continuous(breaks =  c(1:6,rep(NA,4)))
    
 
  
### Growing area
    
    total_growingArea =prettyNum(sum(cropsqn$area, na.rm = T) / 1000, big.mark = ",")
    
    
    ##growing area by country
    
    data_crops_countries = cropsqn %>%
      group_by(country) %>%
      summarise(area = sum(area, na.rm = T)/1000) %>%
      filter(area > 0)
  

    mean_growingA = mean(data_crops_countries$area)

    chart_growing = ggplot(data = data_crops_countries,
           aes (x=reorder(country, area),
                y = area))+
      geom_bar(stat = 'identity',
               fill = blue_IAEA) +
      geom_hline(yintercept = mean_growingA,
                 linetype = "dotted",
                 size = 1,
                 color = gray_dark) +
      geom_label(aes(label= paste("Average:", 
                                 prettyNum(round(mean_growingA, digits = 0), big.mark = ","),
                                 "(thousand ha)"),
                    x = 7, 
                    y = mean_growingA,),
                
                color = second_color,
                nudge_y = 10000,
                size = 5)    +
      
      ##text for growing area
      geom_text(aes(label = prettyNum(round(area,digits = 2), big.mark = ","),
                    y = area,
                    x = country),
                hjust = 0,
                # fontface = 'bold',
                color = second_color,
                nudge_y = 200,
                fontface = 'bold',
                size = 4.5
               
                ) +
      
      coord_flip() +
      ylab("Total cumulative growing area (1,000 ha)") +
      xlab("")+
      labs(#title = "Total growing area of mutant varieties (in 1,000 ha)\nsince 2000. By country",
           caption = caption_online) +
      scale_y_continuous(limits = c(0,25000)) +
      tema
    
     
## Quality traits
    
    ##names of quality traits 
    quality_traits  = names(cropsqn) %>%
      .[str_detect(.,"qual_")] %>%
      str_remove("qual_trait_") %>%
      str_replace_all("\\."," ") %>%
      str_to_lower() %>%
      .[.!="na"] %>%
      knitr::combine_words()
    
    
    ##data for traits 
    to_T = function(x)(if_else(x=="Yes",T,F))
    
    data_quality = cropsqn %>%
      select(crop,starts_with("qual_")) %>%
      select(-qual_trait_NA) %>%
      mutate_if(is.character, list(~na_if(., ""))) %>% ##drop rows missing all variables
      filter(apply(., MARGIN = 1, function(x) sum(is.na(x))) < ncol(.)-1) %>%
      mutate_at(vars(matches("qual_")), to_T) %>%
      group_by(crop) %>% 
      mutate(Total = 1) %>%
      summarise_all(list(sum = sum,
                         average = mean,
                         max = max
                         )) %>%
      mutate(traits = rowSums(.[grep("_max",names(.))])) %>%
      rename(Total_crops = Total_sum)
    
   
    
    number_varieties = nrow(data_quality)
    number_improvedQ = sum(data_quality$traits>0)
    perc_improvedQ = percent(number_improvedQ/number_varieties)
    
    total_improvedQ = sum(data_quality$Total_crops) ## total varieties that improved traits
    
   
    #to number name 
    number_improvedQ = if(number_improvedQ < 10){
                        number_names[number_improvedQ]
    } else{
      number_improvedQ  
      
      }
    
    
    
    
    
    chart_quality = ggplot(data_quality,
           aes(x = traits,
               y = reorder(crop, traits))) +
      geom_bar(stat = "identity",
               fill = blue_IAEA) +
      geom_text(aes(label = traits),
                size= 5,
                fontface = 'bold',
                color = second_color,
                nudge_x =.5)+
      scale_x_continuous(limits = c(0,9),
                         breaks = seq(0,9,1)) +
      ylab("") +
      xlab("Number of quality traits improved") +
      labs(#title = "Number of quality traits reported as improved at least once.\nBy crop",
           caption = caption_online) +
      tema
  
    
    crops_fact = data_quality %>%
      arrange(traits) %>%
      .$crop
    
    
    
    crops_fact= c("Banana",   "Barley",   "Tomato"  , "Bean"  ,   "Chickpea", "Mungbean" ,"Wheat"  , "Rice"  , "Soybean", "Sorghum")
    
    
    data_quality_check = data_quality %>%
      select(crop, ends_with("_average")) %>%
      gather(trait, average, - crop) %>%
      mutate(trait = str_remove_all(trait, "qual_trait_|_average"),
             trait = str_replace(trait,"\\."," "),
             trait = if_else(trait =="High seed.protein.content", "Protein content", trait),
             trait = if_else(trait =="High mineral.content", "Mineral content", trait),
             trait = if_else(trait =="High oil.content", "Oil content", trait),
             frequency = case_when(average == 0 ~ "0",
                                   (average >0 & average < .6) ~ "60%",
                                   average >=.6 ~ "+60%"
                                   ),
             crop = factor(crop,
                           levels = crops_fact))
  
   
    
  
    chart_quality_chek  = ggplot(data = data_quality_check,
           aes(x = trait,
               y = crop, 
               fill = average)) +
      ylab("") +
      xlab("") +
      labs(#title = "Proportion of responses reporting improvement in quality traits.\nBy crop and by quality trait",
           caption = caption_online) +
      geom_tile(color ="white") +
      scale_fill_continuous(high = blue_IAEA, low = light_gray,
                            labels = percent) +
      tema +
      theme(axis.text.x = element_text(angle = 90),
            legend.title = element_blank())
    
    
    #"#132B43"
    
    
### ------------------------------------Criterion 2 : Enhanced environmental protection  ----------------------------
    
  ## Enhaced environmental protection 
    
    environment_traits = "reduction of pesticide use, reduction of chemical fertilizer, increase of water efficiency, or increase soil fertility"
    
    
    environment_data = cropsqn %>%
      select(crop, starts_with("env")) %>%
      gather(trait, value, - crop) %>%
      filter(value != "") %>%
      mutate(trait = str_remove_all(trait, "env_trait_|of|use"),
             trait = str_replace_all(trait, "\\.\\.|\\."," "),
             trait = str_replace(trait, "Reduction", "(-)"),
             trait = str_replace(trait, "Increase", "(+)"),
             Improved = value =="Yes") %>%
      filter(trait != "None") %>%
      group_by(crop, trait) %>%
      summarise(prom = mean(Improved),
                total = sum(Improved)) %>%
      ungroup()%>%
      arrange(total) %>%
      mutate(crop = factor(crop,
                           levels = unique(.$crop)))
    
    
    ##environment chart 
    
    chart_environment = ggplot(data = environment_data,
                                 aes(x = trait,
                                     y = crop, 
                                     fill = prom)) +
      ylab("") +
      xlab("") +
      labs(#title = "Proportion of responses reporting improvement in quality traits.\nBy crop and by quality trait",
        caption = caption_online) +
      geom_tile(color ="white") +
      scale_fill_continuous(high = blue_IAEA, low = light_gray,
                            labels = percent) +
      tema +
      theme(axis.text.x = element_text(angle = 90),
            legend.title = element_blank())
    
    
    
    
 ## Reduction or improvement in environmental protection
    
    data_reduction = cropsqn %>%
      select(crop, reduction_pesticide, reduction_fertilizer, increase_water, increase_soil) %>%
      gather(trait, value, -crop) %>%
      filter(!is.na(value)) %>%
      mutate(trait = case_when(trait == "reduction_pesticide" ~ "(-) pesticide",
                               trait == "reduction_fertilizer" ~ "(-) chemical fertilizer",
                               trait == "increase_water" ~ "(+) water efficiency",
                               trait == "increase_soil" ~ "(+) soil fertility"))
    
    ##get weigth factor
    data_reduction$weigth = data_crops$weight[match(data_reduction$crop, data_crops$crop)]
    
    
    
   
    
    
  
    data_pesticide = data_reduction %>%
      filter(trait == "(-) pesticide")
    
    
    crops_pesticide = combine_words(sort(unique(data_pesticide$crop)))
    number_crops_pesticide = length(unique(data_pesticide$crop))
    average_pesticide = percent(round(mean(data_pesticide$value), digits = 2))
    w_average_pesticide = percent(weighted.mean(data_pesticide$value, data_pesticide$weigth))
    

   
   
   ##transform to number name
    
    number_crops_pesticide = to_number(number_crops_pesticide)
   
    
    chart_pesticide = ggplot(data = data_pesticide,
           aes(x = value,
               y = reorder(crop, value))
           )+
     
      
      geom_vline(aes(xintercept = .08,
                     color = "Good"
      ),
      linetype = "dotted",
      size = 1.2)+
      
      geom_vline(aes(xintercept = .15,
                     color = "Excellent"),
                 linetype = "dotted",
                 size = 1.2) +
                  
        geom_boxplot(color = blue_IAEA,
                     fill = alpha(blue_IAEA,.2)) +
      
      
      scale_x_continuous(labels = percent) +
      scale_color_manual(name = "Criterion", 
                         labels = c("Excellent > (15%)", "Good (8% =< 15%)"),
                         values = c(verde, yellow_IAEA)) +
      ylab("") +
      xlab("Reduction in the use of pesticide") +
      labs(caption = caption_online) +
      
      tema +
      theme(legend.position = "top")
      
  ## chemical fertiliser
    
    data_fertilicer = data_reduction %>%
      filter(trait == "(-) chemical fertilizer")
    
    
    crops_fertilicer = combine_words(sort(unique(data_fertilicer$crop)))
    number_crops_fertilicer = length(unique(data_fertilicer$crop))
    average_fertilicer = percent(round(mean(data_fertilicer$value), digits = 2))
    
    w_average_fertilicer = percent(weighted.mean(data_fertilicer$value, data_fertilicer$weigth))
    
    number_crops_fertilicer = to_number(number_crops_fertilicer)
    
   

   ##Chart fertiliser 
    
    chart_fertiliser = ggplot(data = data_fertilicer,
                             aes(x = value,
                                 y = reorder(crop, value))
    )+
      
      
      geom_vline(aes(xintercept = .1,
                     color = "Good"
      ),
      linetype = "dotted",
      size = 1.2)+
      
      geom_vline(aes(xintercept = .2,
                     color = "Excellent"),
                 linetype = "dotted",
                 size = 1.2) +
      
      geom_boxplot(color = blue_IAEA,
                   fill = alpha(blue_IAEA,.2)) +
      
      
      scale_x_continuous(labels = percent,
                         limits = c(0,.8)) +
      scale_color_manual(name = "Criterion", 
                         labels = c("Excellent > (20%)", "Good (10% =< 20%)"),
                         values = c(verde, yellow_IAEA)) +
      ylab("") +
      xlab("Reduction in the use of chemical fertilizer") +
      labs(caption = caption_online) +
      
      tema +
      theme(legend.position = "top")
    

    
    ##Use of water -------------------------
    
    ## chemical fertiliser
    
    data_water = data_reduction %>%
      filter(trait == "(+) water efficiency")
    
    
    crops_water = combine_words(sort(unique(data_water$crop)))
    number_crops_water = length(unique(data_water$crop))
    average_water = percent(round(mean(data_water$value), digits = 2))
    
    w_average_water = percent(weighted.mean(data_water$value, data_water$weigth))
   
    number_crops_water = str_to_title(to_number(number_crops_water))
    

    
    
    ##Chart fertiliser 
    
    chart_water = ggplot(data = data_water,
                              aes(x = value,
                                  y = reorder(crop, value))
    )+
      
      
      geom_vline(aes(xintercept = .1,
                     color = "Good"
      ),
      linetype = "dotted",
      size = 1.2)+
      
      geom_vline(aes(xintercept = .2,
                     color = "Excellent"),
                 linetype = "dotted",
                 size = 1.2) +
      
      geom_boxplot(color = blue_IAEA,
                   fill = alpha(blue_IAEA,.2)) +
      
      
      scale_x_continuous(labels = percent,
                         limits = c(0,.8)) +
      scale_color_manual(name = "Criterion", 
                         labels = c("Excellent > (20%)", "Good (10% =< 20%)"),
                         values = c(verde, yellow_IAEA)) +
      ylab("") +
      xlab("Increase in water efficiency") +
      labs(caption = caption_online) +
      
      tema +
      theme(legend.position = "top")
    
    
    ##Soil ---------------------------------
    
    
    check = cropsqn %>%
      select(country, crop, increase_soil)
    
    
    data_soil = data_reduction %>%
      filter(trait == "(+) soil fertility")
    
    
    crops_soil = combine_words(sort(unique(data_soil$crop)))
    number_crops_soil = length(unique(data_soil$crop))
    average_soil = percent(round(mean(data_soil$value), digits = 2))
    
    w_average_soil = percent(weighted.mean(data_soil$value, data_soil$weigth))
    
    
    
    number_crops_soil = str_to_title(to_number(number_crops_soil))
    
    
    
    
    
    ##Chart soil ---------------------------------------------------------------------------------
    
    chart_soil = ggplot(data = data_soil,
                         aes(x = value,
                             y = reorder(crop, value))
    )+
      
      
      # geom_vline(aes(xintercept = .1,
      #                color = "Good"
      # ),
      # linetype = "dotted",
      # size = 1.2)+
      # 
      # geom_vline(aes(xintercept = .2,
      #                color = "Excellent"),
      #            linetype = "dotted",
      #            size = 1.2) +
      
      geom_boxplot(color = blue_IAEA,
                   fill = alpha(blue_IAEA,.2)) +
      
      
      scale_x_continuous(labels = percent,
                         limits = c(0,.8)) +
      scale_color_manual(name = "Criterion", 
                         labels = c("Excellent > (20%)", "Good (10% =< 20%)"),
                         values = c(verde, yellow_IAEA)) +
      ylab("") +
      xlab("Increase in soil efficiency") +
      labs(caption = caption_online) +
      
      tema +
      theme(legend.position = "top")
    
    
    
chart_soil

    ### -------------------------------Criterion 3: Strengthened regional capacity and sustainability   ----------------------------------------------------------------------

## When did country started mutation breeding
    
table_members = mainqn %>%
  mutate(`Total years` = 2020 - MB_started) %>%
  select(country, MB_started, `Total years`, Infrastructure_National_team,
         Infrastructure_field_facility, Infrastructure_Radiation_facility) %>%
  rename(`Year mutation breeding started at the national level` = MB_started,
         Country = country,
         `National team`= Infrastructure_National_team,
         `Radiation facility` = Infrastructure_Radiation_facility,
         `Field facility` = Infrastructure_field_facility) %>%
  arrange(desc(`Total years`))


## % of countries with national teams, and access to field and radiation facilities
mean_national_team =paste0(round(mean(table_members$`National team`=="Yes") *100, digits = 1), "%")
mean_field_facility =paste0(round(mean(table_members$`Field facility`=="Yes") *100, digits = 1), "%")
mean_radiation_facility =paste0(round(mean(table_members$`Radiation facility`=="Yes") *100, digits = 1), "%")


## Training courses un mutation breeding 

courses_conducted = sum(internal2$traings_conducted, na.rm = T)

##People trained 

people_trained = sum(internal2$people_trained, na.rm = T)
women_trained = sum(internal2$women_trained, na.rm = T)
women_trained_perc = paste0(round((women_trained/people_trained)*100, digits = 1),"%")
mean_trained = round(mean(internal2$people_trained, na.rm = T), digits = 0)

##chart of people trained by country

 chart_trained = ggplot(data = internal2 %>%
           select(country, people_trained) %>%
           arrange(people_trained),
         aes(x=reorder(country, people_trained),
             y = people_trained)) +
    geom_bar(stat = 'identity',
             fill = blue_IAEA) +
   geom_text(aes(label = people_trained),
             size = 5,
             nudge_y = 2,
             color = second_color) +
    coord_flip() +
    xlab("") +
    ylab("People trained under RCA since 2000") +
    labs(#title = "Total number of people trained under RCA projects since 2000.\nBy country",
         caption = caption_internal) +
   tema
   
chart_trained

##Countries with personnel trained:

 personnel_trained_i = internal2 %>%
   group_by(country) %>%
   summarise(personnel = sum(people_trained)>0)
 
 personnel_trained = mainqn %>%
   group_by(country) %>%
   summarise(personel_o = sum(people_trained)>0) %>%
   right_join(personnel_trained_i) %>%
   mutate(trained = rowSums(.[c("personnel", "personel_o")], na.rm = T))
   
 countries_trained = sum(personnel_trained$trained>0)
 
 
 countries_not_trained = combine_words(personnel_trained$country[personnel_trained$trained==0])
 

 
 ### Networking, collaboration, and knowledge transfer 
 
 expert_missions = sum(internal2$expert_missions, na.rm = T)
 expert_attendees = sum(internal2$expert_attendees, na.rm = T)
  expert_women = paste0(round(sum(internal2$expert_women, na.rm = T) / expert_attendees, digits = 2) *100, "%")
 
 
 chart_experts =  ggplot(data = internal2 %>%
                           select(country, expert_attendees) %>%
                           filter(expert_attendees>0),
                         aes(x=reorder(country, expert_attendees),
                             y = expert_attendees)) +
   geom_bar(stat = 'identity',
            fill = blue_IAEA) +
   geom_text(aes(label = expert_attendees),
             size = 5,
             nudge_y = .5,
             color = second_color)+
   coord_flip() +
   xlab("") +
   ylab("Total number of national experts who have attended expert missions") +
   labs(#title = "Total number of experts that have joined expert missions to other\ncountries under RCA since 2000",
        caption = caption_internal) +
   tema

 chart_experts 
 
 ## workshops / meetings
 
 workshops = sum(internal2$workshops, na.rm = T)
 workshop_participants = sum(internal2$worskshop_participants, na.rm = T)
 
 
 ##Publications 
 
 sum(mainqn$publications, na.rm = T)
 
 data_publication = mainqn %>%
   filter(publications >0) %>%
   select(country, publications, publications_sci) %>%
   mutate(`Non scientific` = publications - publications_sci) %>%
   rename(Scientific = publications_sci) %>%
   select(-publications) %>%
   gather(value = "value", key="Type", -country) %>%
   filter(country !="China")
 
 
chart_publications =  ggplot(data = data_publication,
        aes(x=reorder(country,value),
            y = value,
            fill = Type)) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   xlab("") +
   ylab("Number of publications since 2000") +
   labs(#title = "Number of publications in mutation breeding since 2000.\nBy publication type and country",
        caption = caption_online) +
   scale_fill_manual(values = c(blue_IAEA, second_color)) +
   tema +
   theme(legend.position = "top",
         legend.title = element_blank())
 
 
 
 
 publications = sum(data_publication$value)
 scientific_publications = sum(data_publication[data_publication$Type == "Scientific",]$value)
 perc_scientific = paste0(round((scientific_publications/publications)*100, digits = 1), "%")
 
 publications = prettyNum(publications, big.mark = ",")

## Knowledge and services
 
 knowledge_total = sum(mainqn$knowledge_share == "Yes")
 knowledge_perc = paste0(round(mean(mainqn$knowledge_share == "Yes") *100, digits = 0),"%")

 countries_knowledge = knitr::combine_words(mainqn[mainqn$knowledge_share == "Yes", ]$country)

 
  to0 <- function(x) (ifelse(x=="Yes",1,0))  ##function to convert Yes to 1
 
  ##data for visualization of knowledge shared
 data_know = mainqn %>%
   select(starts_with("Shared")) %>%
   mutate_at(vars(matches("Shared")),to0) %>%
   gather(Service, value) %>%
   mutate(Service = str_remove(Service, "Shared_"),
          Service = str_remove(Service,"\\.")) %>%
   group_by(Service) %>%
   summarise(Total = sum(value)) %>%
   mutate(label = paste(Total,Service)) %>%
   arrange(Total) %>%
   mutate(Service  = factor(Service,
                            levels = unique(.$Service)))
 
  ## Chart of services shared by country 
 chart_know = ggplot(data = data_know,
        aes(x = Total,
            y = Service))+
   geom_segment(aes(x = 0,
                    xend = Total,
                    y = Service,
                    yend = Service),
                size =1.5,
                color = second_color) +
   geom_point(size = 9,
              fill = blue_IAEA,
              color = blue_IAEA,
              )+
   geom_text(aes(label = Total),
             color = second_color,
             fontface ='bold')+
   ylab("")+
   xlab("Number of countries") +
   labs(#title = "Number of countries that have shared knowledge with other\ncoutries (by type of knowledge shared)",
        caption = caption_online) +
   tema +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank())
 
 

##Institutions and funders
 
 institutions  = sum(mainqn$institutions)
 funders = sum(mainqn$funders)

 data_inst = mainqn %>%
   select(country, institutions, funders) %>%
   gather(partner, value, - country) 
 
 
 
 chart_partners = ggplot(data = data_inst,
        aes(x = reorder(country, value),
            y = value,
            fill = partner
            )
        )+
   geom_bar(stat =  'identity',
            position = 'dodge') +
   coord_flip() +
   xlab("") +
   ylab("Number of institutions/partners") +
   scale_fill_manual(values = c(blue_IAEA, second_color),
                     labels = c("Donors", "Institutions/Companies")) +
   labs(fill = "Partners",
        #title = "Number of institutions and donors that have cooperated for\nmutation breeding since 2000 by country and type of partnership",
        caption = caption_online) +
   tema +
   theme(legend.position = "top")
 
 
 

 