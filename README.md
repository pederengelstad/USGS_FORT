# Species Occurrence Data Aggregation Scripts

Overview of code files:
* API_Sources: pull occurrence records from online repositories of species occurrence data (i.e. GBIF, BISON, EDDMapS).
* Data_Cleaning: de-duplicate, filter, and clean occurrence records for various geospatial errors.
* DataFromFiles: pull occurrence records from hard coded files (i.e. agency records).
* OccurrenceData_Main: shell script that calls all other scripts to process the data in order.
* SpeciesProcessing: gather accepted names and synonyms of species based on [ITIS](https://itis.gov/) taxonomy.
* target_background_generation: generates a list of all known introduced species (L48 only) from [USDA PLANTS](https://plants.sc.egov.usda.gov/).

[![DOI](https://zenodo.org/badge/133727553.svg)](https://zenodo.org/badge/latestdoi/133727553)
