
createDatasetsTemplate <- function(template_filename="datasets.csv", readme_filename="datasets.txt"){

  datasets <-
    data.frame(
      "dataset_name"=NA,
      "dataset_path"=NA,
      "dataset_format"=NA,
      "dataset_type"=NA)

  write.csv(
    datasets,
    template_filename,
    na="",
    quote=FALSE,
    row.names=FALSE)

  cat(paste("Import Template written to ", template_filename, ".\n", sep=""))

  readme <-
'
Readme for datasets template
============================

"dataset_name": Name of the dataset. Must be unique within a study.

"dataset_path": Path to the dataset.

"dataset_format: Format of the file:
* Stata
* SPSS
* CSV

dataset_type:
* "cross": Cross-sectional dataset.
* "long":  dataset in the long-format.
* "spell": Spell dataset.
'

  write(readme, readme_filename)

  cat(paste("Readme file written to ", readme_filename, ".\n", sep=""))

}



