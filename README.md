## extract_traits_from_text

A method for generating plant trait tables from flora textual descriptions

This technique includes code used to download flora descriptions from the Vicflora online resource (1) (1_download_taxon_descriptions.R). Flora and herbarium resources are constantly updating and innovating. Since I downloaded the descriptions shown here last year, the format of the Vicflora website has changed and the webscraping code posted here no longer works. However, the sample text of 300 taxa can be used to illustrate the trait extraction technique vic_flora_downloaded.csv. The following resources are useful starting points to learn about webscraping techniques and etiquette:
https://rvest.tidyverse.org/articles/rvest.html,
https://www.datacamp.com/tutorial/r-web-scraping-rvest,
https://www.r-bloggers.com/2020/04/tutorial-web-scraping-in-r-with-rvest).
or for wrangling and extracting data from pdf text format:
https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

The trait extraction workflow takes a .csv file of plant taxa names and their descriptions from 300 taxa entries in Vicflora online (vic_flora_downloaded.csv), splits each sentence and phrase into categories of plant form, stem,, leaf, flower, fruit, seed, root (2_sentence_splitting.R) and extracts some simple plant traits relating to plant form (3_extract_forms.R).

The sentence splitting technique uses a lookup table of more than 200 botanical terms to categorise each sentence (topic_lookup_table.csv). It takes a minute or two for the 2_sentence_splitting.R code to run.

Traits are then extracted using the terms and trait names defined in the traits_form.csv file. 3_extract_forms.R can also take up to a minute to run.

Finally, trait values that are scored from the family and genus descriptions are used to score some constituent taxa in the 4_inferring_traits.R script. Please see the published resource for more conceptual details about the process.  

## References
1. Vicflora 2022. Flora of Victoria, Royal Botanic Gardens Victoria.
Available online: https://vicflora.rbg.vic.gov.au
Viewed 04 September 2022.
