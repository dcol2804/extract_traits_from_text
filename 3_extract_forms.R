##################################################

##############  Load packages ####################

##################################################

library(tidyverse)

##################################################

##############  Define functions #################

##################################################

# This function cleans the raw flora descriptions and prepares it for the sentence splitting function

remove_phrases = function(a){
  
  a$text = a$form
  # remove the meaningful words surrounding disqualifiers such as "not xxxx" 
  a$text = gsub("\\((.*?), not in australia(.*?)\\)", "", a$text)
  a$text = gsub("and not in australia(.+?)[[:punct:]]", "", a$text)
  a$text = gsub("\\w+ \\(not in australia\\)", "", a$text)
  a$text = gsub("\\w+ \\(non-australia\\)", "", a$text)
  a$text = gsub("non-\\w+", "", a$text)
  a$text = gsub("\\?\\w+", "", a$text)
  a$text = gsub("\\w+ absent", "", a$text)
  a$text = gsub("\\w+ not observed", "", a$text)
  a$text = gsub("[Nn]ot \\w+", "", a$text)
  a$text = gsub("without \\w+", "", a$text)
  a$text = gsub("without a \\w+", "", a$text)
  a$text = gsub("\\w+-like", "", a$text)
  a$text = gsub("rather than .*", "", a$text)

  # next remove problematic annual/perennial distinctions
  a$text = gsub("annual above ground parts", "", a$text)

  # collapse two word combinations ready for extraction
  a$text = gsub("tree fern|tree-fern", "treefern", a$text)
  a$text = gsub("stem-parasit|stem parasit", "stemparasit", a$text)
  a$text = gsub("root-parasit", "rootparasit", a$text)
  a$text = gsub("hemi-parasit", "hemiparasit", a$text)
  a$text = gsub("hemi-epiphyt", "hemiepiphyt", a$text)
  a$text = gsub("short lived perennial|short-lived perennial", "shortlivedperennial", a$text)
  a$text = gsub("semi-aquatic", "semiaquatic", a$text)
  a$text = gsub("free-floating|free floating", "freefloating", a$text)
  a$text = gsub("woody twiner", "woodytwiner", a$text)
  a$text = gsub("shrubby twiner", "shrubbytwiner", a$text)
  a$text = gsub("woody climber", "woodyclimber", a$text)
  a$text = gsub("woody vine", "woodyvine", a$text)
  a$text = gsub("climbing herb", "climbingherb", a$text)
  a$text = gsub("herbaceous climber", "herbaceousclimber", a$text)
  a$text = gsub("herbaceous perennial", "perennial herb", a$text)
  a$text = gsub("herbaceous to woody perennial", "perennial herbs or shrubs", a$text)
  a$text = gsub("woody or herbaceous vines|herbaceous to semi-woodyvine|herbaceous or woodyvines|climbers with woody or herbaceous stems|herbaceous or woody climbers|vines or woody-based climbers", "herbaceousclimber, woodyclimber", a$text)
  
  #remove exact terms that interfere
  a$text = gsub("ascending tree|tree hollows|tops of trees|tree bole|trunks of treeferns|climbing to tree-tops|base of trees|axillary bud.*|with plank-buttress.*|tree-top|to tree or treefern|on tree trunks|usually high in trees", "", a$text)

  # add a space on the ends ready for the extraction
  a$text = str_c(" ", a$text, " ")
  
  return(a)
  
}

##################################################

#### Preparation for the traits_form.csv file ####

##################################################

# read in the terms to extract and prepare them for the extraction as in the sentence splitting code
t = read.csv("traits_form.csv")
t$regex = str_c("[\\s[:punct:]]", t$extract, "[\\s[:punct:]s]")
t = t %>% filter(extract != "")

# generate a template of all possible trait_names
trait_table = data.frame(trait_name = c(str_c(unique(t$trait_name), "_a"), 
                                        str_c(unique(t$trait_name), "_f")), blank = NA) %>% arrange(trait_name)

trait_table_wide = trait_table %>% pivot_wider(names_from = trait_name, values_from = blank)

##################################################

#####  Start of trait extraction process #######

##################################################

# read in the floras which have been divided into the categories
data = read.csv("vic_flora_split_sentences.csv", encoding="latin1")

# clean the data with the remove_phrases function
data =  remove_phrases(data)

# create a new blank dataframe to store the output
new = data.frame()

  for (i in 1:length(data$text)){
  
  test_out = data.frame()

  # for each trait_name category (plant_growth_form, parasitic etc)
  for (j in 1:length(t$regex)){
    
  # is there a match to any of the trait_name categories (T or F)?
   test = str_detect(data$text[i], t$regex[j])
   
   # what was the match?
   test2 = str_extract(data$text[i], t$regex[j])
   
   #clean the output
   test2 = str_trim(test2)
   test2 = gsub("[[:punct:]]", "", test2)
   
   # make a dataframe
   test3 = data.frame(match = test, flora_term = test2)
   
   # glue it to the end to make a dataframe of equal length to t
   test_out = rbind(test_out, test3)
   
  }
  
  # use the logical vector (match column) and the extracted terms to create a dataframe of just trait_names where there was a match
  temp = t %>% filter(test_out$match) %>% select(-regex) %>% 
                           mutate(flora_term = test_out$flora_term[!is.na(test_out$flora_term)]) %>% 
                           unique()
  
  # If no traits exist, make a blank row with just the original text if no words at all were extracted and start the for loop again (go to the next taxon).
  if (length(temp$flora_term) == 0){
 
   out_final = cbind(data.frame(taxon_name = data$taxon_name[i], original_text = data$original_text[i]), text = data$text[i], trait_table_wide)
    
  new = rbind(new, out_final)
  
  # go to the next taxon_name and description
  next()
  
  }
  
  # Split into dataframes of different traits
  out_list <- split(temp, f = temp$trait_name)
  
  #create a blank dataframe
  out = data.frame()
  
  for (k in 1:length(out_list)){
    
    trait = out_list[[k]]
    
    # create a dataframe with the traits_name, a classification column labeling the austraits term (a) or the flora term (f) and the trait values
    df = data.frame(trait_name = names(out_list[k]), 
                    value_type = c("a", "f"), 
                    values = c(str_c(unique(trait$austraits), collapse = " "), str_c(trait$flora_term, collapse = " "))
    )

    
    out = rbind(out, df)
  }
  
  # assign each row as either the austraits trait_name: "trait_name_a" or the flora trait_name: "trait_name_f", ready for pivot_wider
  out = out %>% mutate(trait_name = str_c(trait_name, "_", value_type)) %>% select(-value_type)
  
  # merge with a template containing all possible traits so as to preserve the number of columns even if some categories will be blank
  traits = merge(trait_table %>% select(trait_name), out, by = "trait_name", all.x = T)
  
  # turn it into a wide table for attaching to the original text. 
  out_final = pivot_wider(traits, names_from = trait_name, values_from = values)
  
  out_final = cbind(taxon_name = data$taxon_name[i], 
                    original_text = data$original_text[i], 
                    text = data$text[i],
                    out_final)
  
  # store the finished row of data
  new = rbind(new, out_final)
  
  }

##################################################

##############  Save the data ####################

##################################################  

write.csv(new, "vic_flora_form_traits.csv", row.names = F, fileEncoding="Windows-1252", na = "")



