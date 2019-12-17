# Species Occurrence Data Aggregation Scripts

USGS disclaimer:
This software has been approved for release by the U.S. Geological Survey (USGS).
Although the software has been subjected to rigorous review,
the USGS reserves the right to update the software as needed pursuant to further analysis and review.
No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality
of the software and related material nor shall the fact of release constitute any such warranty.
Furthermore, the software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or unauthorized use.

Overview of code files:
* API_Sources: pull occurrence records from online repositories of species occurrence data (i.e. GBIF, BISON, EDDMapS).
* Data_Cleaning: de-duplicate, filter, and clean occurrence records for various geospatial errors.
* DataFromFiles: pull occurrence records from hard coded files (i.e. agency records).
* OccurrenceData_Main: shell script that calls all other scripts to process the data in order.
* SpeciesProcessing: gather accepted names and synonyms of species based on [ITIS](https://itis.gov/) taxonomy.
* target_background_generation: generates a list of all known introduced species (L48 only) from [USDA PLANTS](https://plants.sc.egov.usda.gov/).

For questions or more information contact: [Peder Engelstad](mailto:peder.engelstad@colostate.edu) at Colorado State University.

[![DOI](https://zenodo.org/badge/133727553.svg)](https://zenodo.org/badge/latestdoi/133727553)
