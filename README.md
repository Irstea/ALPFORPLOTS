# Data paper for the forest plots of Irstea Grenoble in revision in Ecology

Repository with original and updated data for the data paper
'Long-term tree inventory data from mountain forest plots in France'
Fuhr et al. *Ecology*. The data are provided in the folder `output`
and R code to replicate the figure, table in folder 'R' and manuscript
in folder 'ms'. Updated version will be available with new commit with each new
version identified by commit message with the version number of the
update (current version 1.0.10). Version numbers will follow the semantic
 versioning guidelines.
 
This code reads the data, generates the tables and plots of the manuscript.

The following files are provided in the folder `output`:

- `data_c.csv`: Tree coordinates file
- `data_m.csv`: Tree measurements file
- `data_p.csv`: Plots description file
- `metadata_data_c.csv`: Metadata for tree coordinates file
- `metadata_data_m.csv`: Metadata for tree measurements file
- `metadata_data_p.csv`: Metadata for plots description file
- `species_code.csv`: Correspondence between species code and Latin name
- `status_code.csv`: Definition of status code


In addition the code computes growth and do several quality check and generate the html of the metadata description.


## Installation


The following code work with `remake` which can be installed directly in R

- remake: `devtools::install_github("richfitz/remake")`


## Folder structure

- `output` formatted data and table generated for the paper.
- `ms` mardown file for metadata
- `R` R functions used.

