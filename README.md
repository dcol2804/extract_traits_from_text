## extract_traits_from_text

A method for generating plant trait tables from flora textual descriptions

This technique takes a .csv file of plant taxa names and their descriptions from 3000 taxa entries in Flora of Australia online (Flora_of_Australia.csv), splits each sentence and phrase into categories of plant form, stem,, leaf, flower, fruit/seed, root and substrate/location (1_sentence_splitting.R) and extracts some simple plant traits relating to plant form (2_extract_forms.R). 

The sentence splitting technique uses a glossary of more than 200 botanical terms to categorise each sentence (glossary.csv)

Traits are then extracted using the terms and trait names defined in the traits_form.csv file


## References
1. Flora of Australia Online. Australian Biological Resources Study, Canberra. Viewed 04 September 2022.
http://www.environment.gov.au/science-research/abrs/online-resources/flora-of-australia-online
