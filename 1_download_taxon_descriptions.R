library(tidyverse)
library(rvest)
library(xml2)

url = "https://vicflora.rbg.vic.gov.au/flora/taxon/"

#download the spreadsheet from this website for all trachyophytes: 
vic_flora = read.csv("taxon_list_vic_flora.csv", stringsAsFactors = F)

vic_flora = vic_flora %>% filter(taxon_rank != "order") %>% 
  filter(taxon_rank != "superorder") %>% 
  filter(taxon_rank != "phylum") %>% 
  filter(taxonomic_status == "accepted")


x = download(urls[i], "out.txt")

urls = str_c(url, vic_flora$id)
#output1 = output

output = data.frame()

for (i in 1:length(urls)){
  
  webpage = read_html(urls[i])

  data_html = html_nodes(webpage, "div")
  data_status = html_text(data_html)
  data_status1 = paste0(data_status, c("", ","))
  data_status2 <- paste(data_status1, collapse=" ")
  if (length(data_status2) == 0){
    data_status2 = NA 
  }
  
  
  data_html = html_nodes(webpage, ".description p")
  data = html_text(data_html)
  data <- paste(data, collapse=" ")
  if (length(data) == 0){
    data = NA
  }

  data_html = html_nodes(webpage, ".distribution-habitat p")
  data_dh = html_text(data_html)
  data_dh <- paste(data_dh, collapse=" ")
  if (length(data_dh) == 0){
    data_dh = NA
  }
  
  data_html = html_nodes(webpage, "p")
  data_notes = html_text(data_html)
  data_notes <- paste(data_notes, collapse=" ")
  if (length(data_notes) == 0){
    data_notes = NA
  }  


  data_html = html_nodes(webpage, ".profile-source")
  data_ps = html_text(data_html)
  data_ps <- paste(data_ps, collapse=" ")
  if (length(data_ps) == 0){
    data_ps = NA
  }  
  
  
  data_html = html_nodes(webpage, ".created-by")
  data_cb = html_text(data_html)
  data_cb = gsub("\r\n","",data_cb)
  data_cb = str_trim(data_cb)
  if (length(data_cb) == 0){
    data_cb = NA
  }
  
  
  
  x = data.frame(url = urls[i], statuses = data_status2, description = data, dist_habitat = data_dh, notes = data_notes, source = data_ps, author = data_cb)
  
  output = rbind(output, x)
}

vic_flora$taxon_name = vic_flora$scientificName
vf_useful = vic_flora %>% select(taxon_name, taxonRank)
output1 = cbind(vf_useful, output)


write.csv(output1, "Vic_flora_downloaded.csv")
