
##################################################

##############  Load packages ####################

##################################################

library(tidyverse)

##################################################

##############  Define functions #################

##################################################

# This function cleans the raw flora descriptions and prepares it for the sentence splitting process

flora_cleaning = function(q){
  
  
  #Take out unnecessary rows and phrases
  q = q %>% filter(!is.na(description))
  q = q %>% filter(!str_detect(description, "Pending"))
  
  # store a copy of the original text
  q$original_text = q$description
  
  # Preparation for sentence splitting:
  # remove any abbreviated genus names so the full stop isn't mistaken for the end of a sentence
  q$description = gsub("[A-Z]\\. ", "", q$original_text)
  
  # convert to lower case
  q$description = str_to_lower(q$description)
  
  # make more amendments to ensure the splitting occurs only at the end of a sentence or phrase
  q$description = gsub(" c\\.|\\(c\\.", " circa", q$description)
  q$description = gsub(" ca\\.", " circa", q$description)
  q$description = gsub("i\\.e\\.", "i e", q$description)
  q$description = gsub("subsp\\.", "subsp", q$description)
  q$description = gsub("var\\.", "var", q$description)
  q$description = gsub("e\\.g\\.", "e g", q$description)
  q$description = gsub("fig\\.", "figure", q$description)
  
  # If there's a full stop and no space between each of the glossary terms and the next sentence (a typo), add in a space so the sentence splitting works
  for (i in 1:length(g$words)){
    q$description = gsub(str_c("\\.", g$words_original[i]), str_c(". ", g$words_original[i]), q$description)
  }
  
  # If required, change and standardise glossary terms in the text to make them unique/distinctive from other uses of those words
  q$description = gsub("pollen cone", "pollencone", q$description)
  q$description = gsub("flower stem", "flowerstem", q$description)
  q$description = gsub("flowering stem", "floweringstem", q$description)
  q$description = gsub("stem lea", "stem-lea", q$description)
  q$description = gsub("inflorescence-bearing", "Inflorescencebearing", q$description)
  q$description = gsub("leaf scars", "leaf-scars", q$description)
  q$description = gsub("vegetative culms", "Vegetative culms", q$description)
  q$description = gsub("flowering plant", "floweringplant", q$description)
  q$description = gsub("root system", "rootsystem", q$description)
  return(q)
}



##################################################

#####  Preparation for the glossary.csv file #####

##################################################

# read in the glossary and prepare the categories and word searches
g = read.csv("topic_lookup_table.csv")
g = data.frame(words_original = c(g$singular, g$plural), organ = g$Plant_organ)
g = g %>% filter(words_original != "") 

# make all the search terms in the glossary.csv file only match to a space or punctuation at the start and end of the word and a plural "s" at the end of the word
g$words = str_c("[\\s[:punct:]]", g$words_original, "[\\s[:punct:]s]")

# create a unique list of categories that exist in the glossary. Should be edited to reflect the names and order of the categories that you would like for the output
tissue = c("form", "stem", "leaf", "flower", "fruit","seed", "root")
tissue_table = data.frame(organ = tissue)

# create a vector of merged strings to search for each category
word_searches = sapply(tissue, function(y){paste(g$words[g$organ == y], collapse="|")})

# Add on one extra category for sentences you would like to not match to any category (be assigned to the category of the previous sentence)
word_searches1 = c(word_searches, "hairs|gland|margins|nodes|lobes|indumentum")
names(word_searches1) = c(tissue, "blank")


##################################################

#####  Start of sentence splitting process #######

##################################################


  #read in the data
  data = read.csv("vic_flora_downloaded.csv", stringsAsFactors = F, encoding = "latin1", header = T)
  
  # Run the functions on the data
  data = flora_cleaning(data)
  
  # The following code:    1) splits each paragraph of text into sentences/phrases delineated by ". " or "; " and assigns a number to record the original order
  #                        2) searches each phrase for all the words in the glossary and assigns the phrase to a category, either whole plant, stem, root etc.
  #                        3) If multiple glossary words match a phrase, the phrase is assigned the category of the first mentioned glossary word.
  #                        4) If no glossary words match, the phrase is assigned to the previous sentence. If the phrase is the first phrase of the whole paragraph, it is assigned to plant_growth_form
  #                        5) Cleans and collapses the sentences in each category back into text again
  
  #create a blank dataframe to store the result
  out = data.frame()
  
  # loop through each description in the flora
  for (i in 1:length(data$description)){
    
    #Step 1
    # Split the text into sentences and phrases
    t = unlist(str_split(data$description[i], "(?<=[\\;\\.])\\s"))
    t = str_c(" ", t, " ")
    
    # number them
    y = data.frame(number = 1:length(t), sentence = t)
    
    # Step 2
    # Search for words in the glossary, assign a category and return to their original order in the text
    single = data.frame()
    
    for (z in 1:length(tissue)){
      
      single1 = y %>% filter(str_detect(sentence, word_searches[z])) %>% mutate(organ = tissue[z]) %>% arrange(number)
      
      single = rbind(single, single1)
      
    }
    
    # Step 3
    # Assign sentences that are tagged as cntaining more than one word to a category
    multiple = single %>% filter(number %in%  single$number[duplicated(single$number)])
    
    #if there are sentences with words from multiple categories in them:
    if (nrow(multiple) != 0){
      
      # isolate unquee sentences
      s = multiple %>% select(number, sentence) %>% unique()
      
      # create a blank dataframe
      keep = data.frame()
      
      for (n in 1:length(s$sentence)){
        
        # split the sentence up into words
        x1 = unlist(str_split(s$sentence[n], " "))
        
        #add spaces on each word
        x1 = str_c(" ", x1, " ")
        
        # number them
        y1 = data.frame(number = 1:length(x1), word = x1)
        
        # create a blank dataframe
        multi = data.frame()
        
        # assign the correct category to each word 
        for (z in 1:length(word_searches1)){
          
          multi = rbind(multi,  y1 %>% filter(str_detect(word, word_searches1[z])) %>% mutate(organ = names(word_searches1)[z])) %>% arrange(number)
          
        }
        
        # make a dataframe for the sentence number, the sentence itself and the the first category mentioned in the sentence
        multi = data.frame(number = s$number[n], sentence = s$sentence[n], organ = multi$organ[1])
        
        
        # If these sentences mention hairs, glands, lobes, margins etc they should be removed from this step and processed as if they had no matches with any glossary words. 
        
        if (multi$organ != "blank"){
       
          # keep the sentences
           keep = rbind(keep, multi)
        
        }else{
          
          # Take them out of this step. They will be processed as not having a match (in the next step)
          single = single %>% filter(number != multi$number)
        
          }
      }
      
      # remove the multiple matched sentences from the original "single" table and bind on the correctly categorised sentences
      single = single %>% filter(!number %in%  single$number[duplicated(single$number)]) %>% rbind(keep)
    }
    
    #################################################################################### 
    
    # Step 4
    # Assign sentences that contain no glossary words to the category of the previous sentence
    none = y %>% filter(!number %in% single$number) %>% mutate(organ = NA)
    
    # only assign to an organ if there are unassigned sentences
    if(nrow(none) != 0){
      
      for (n in 1:nrow(none)){
        
        # if the unassigned sentence is the first sentence, assign it to the form category
        if (none$number[n] == 1){
          
          none$organ[n] = "form"
          single = rbind(single, none[n,]) %>% arrange(number)
          
        }else{
          
          # assign to the category of the previous sentence
          none$organ[n] =  single$organ[which(single$number == (none$number[n] - 1))]
          
          # bind this row onto the original dataframe called "single" 
          single = rbind(single, none[n,]) %>% arrange(number)
          
        }
      }
      
    }
    
    # Step 5
    # Rearrange and remove excess spaces
    single = single %>% arrange(organ, number)
    single$sentence = str_trim(single$sentence)
    single$sentence = str_squish(single$sentence)
    
    #################################################################################
    # collapse the sentences with common category to create the text
    
    # Makes a list of dataframes based on category
    out_list <- split(single, f = single$organ)
    
    # Create the collapse back function - return the separate phrases within a category to a chunk of text
    collapse_back = function(t){
      
      v = data.frame(organ = t$organ[1], sentence = str_c(t$sentence, collapse = " "))
      
    }
    
    # apply it to each category and make it into a dataframe
    single = bind_rows(lapply(out_list, collapse_back))
    
    # merge onto the template of possible categories ( to preserve all the categories, even if some are blank for this particular row)
    single = merge(tissue_table, single, by = "organ", all.x = T)
    
    # make the data a wide table with a category in each column
    single = pivot_wider(single, names_from = organ, values_from = sentence)
    
    # add the original text back in
    single$description = str_trim(data$description[i])
    single$taxon_name = str_trim(data$taxon_name[i])
    single$original_text = data$original_text[i]
    
    out = rbind(out, single)
    
  }
  
  # Create a neat table with the columns in the order you wish
  out = out[, c("taxon_name", "original_text", "description", tissue)]
  
  
##################################################
  
##############  Save the data ####################
  
##################################################  
  
  
# Create a csv file of the same
write.csv(out, "Vic_flora_split_sentences.csv", row.names = F, fileEncoding="Windows-1252", na = "")
  






