

infer_form = function(file){
  
  
  a = read.csv(file, encoding="latin1")
  
  for (i in 1:length(traits)){
    
    aus = str_c(traits[i], "_a")
    flo = str_c(traits[i], "_f")
    #take out section, series, subg
    a = a %>% filter(!str_detect(taxon_name, "sect\\.|ser\\.|subg\\.|Group|Subgroup"))
    
    # make the family and genus divisions
    family = a %>% filter(str_detect(taxon_name, "eae$"), !str_detect(taxon_name, "\\s"))
    genus = a %>% filter(!str_detect(taxon_name, "\\s"), !taxon_name %in% family$taxon_name)
    
    # adding family to genus forms
    family_single = family %>% filter(str_detect(.data[[aus]], "^\\w+$"))
    family_single = merge(family_single, family_index, by.x = "taxon_name", by.y = "Family")
    ge = genus %>% filter(.data[[aus]] == "")
    ge_filled  = merge(ge %>% select(-.data[[aus]], -.data[[flo]]), family_single %>% select(Genus, .data[[aus]], .data[[flo]]), by.x = "taxon_name", by.y = "Genus")
    
    if (nrow(ge_filled)> 0){
      
      ge_filled[[flo]] = "inferred_from_family"
      
    }
    
    # add them back in to both the genus and the original datasets
    genus = rbind(genus %>% filter(!taxon_name %in% ge_filled$taxon_name), ge_filled)
    a = rbind(a %>% filter(!taxon_name %in% ge_filled$taxon_name), ge_filled)
    
    # Adding the growth form of a genus with only one growth form to anything with that genus without one
    genus_single = genus %>% filter(str_detect(.data[[aus]], "^\\w+$"))
    sp = a %>% filter(str_detect(taxon_name, "\\s"), .data[[aus]] == "")
    sp = sp %>% mutate(genus_root = str_extract(taxon_name, "^\\w+"))
    sp_filled = merge(sp %>% select(-.data[[aus]], -.data[[flo]]), genus_single %>% select(taxon_name, .data[[aus]], .data[[flo]]), by.x = "genus_root", by.y = "taxon_name", all = F) %>% select(-genus_root)
    
    if (nrow(sp_filled)> 0){
      
      sp_filled[[flo]] = ifelse(sp_filled[[flo]] %in% c( "inferred_from_family"), sp_filled[[flo]], "inferred_from_genus")
      
    }
    
    # add them back in 
    a = rbind(a %>% filter(!taxon_name %in% sp_filled$taxon_name), sp_filled)
    
    # annual or perennial genus description
    species = a %>% filter(str_detect(taxon_name, "^\\w+\\s\\w+$| sp\\. [A-Z]"))
    
    # All the scientific names below species level
    species_single = species %>% filter(str_detect(taxon_name, "^\\w+\\s\\w+$"), str_detect(.data[[aus]], "^\\w+$"))
    subsp = a %>%  filter(str_detect(taxon_name, " subsp[\\.\\s]| var[\\.\\s]| f\\."), .data[[aus]] == "")
    subsp = subsp %>% mutate(species_root = str_extract(taxon_name, ".*(?= subsp[\\.\\s])|.*(?= var[\\.\\s])|.*(?= f\\.)"))
    subsp_filled = merge(subsp %>% select(-.data[[aus]], -.data[[flo]]), species_single %>% select(taxon_name, .data[[aus]], .data[[flo]]), by.x = "species_root", by.y = "taxon_name", all = F) %>% select(-species_root)
    
    if (nrow(subsp_filled)> 0){
      subsp_filled[[flo]]  = ifelse(subsp_filled[[flo]] %in% c("inferred_from_family","inferred_from_genus"), subsp_filled[[flo]], "inferred_from_species")
    }
    
    
    a = rbind(a %>% filter(!taxon_name %in% subsp_filled$taxon_name), subsp_filled)
    
  }
  
  a = unique(a)
  
  return(a)
  
}



###################################################################################

# load and create the family/genus reference list
APC = read.csv("APC-taxon-2023-02-21-2824.csv") %>% filter(taxonomicStatus == "accepted")

family_index = APC %>% select(scientificName, higherClassification) %>% mutate(below_family = str_extract(higherClassification, "\\w+ceae.*")) %>% separate("below_family", c("Family", "Genus", "Species", "Subsp"), sep =  "\\|")
family_index = family_index %>% select(Family, Genus) %>% filter(!is.na(Genus)) %>% unique()

# load the trait names that you want to infer from
traits = c("plant_growth_form", "life_history","plant_growth_substrate", "sex_type")

#load the list of files
file = "vic_flora_form_traits.csv"


forms = infer_form(file)

write.csv(forms, "vic_flora_forms_final.csv",  row.names = F, fileEncoding="Windows-1252", na = "")
