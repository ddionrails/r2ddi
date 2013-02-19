
createDatasetsTemplate <- function(template.filename="datasets.csv", readme.filename="datasets.txt"){

  datasets <-
    data.frame(
      "Dataset.name"=NA,
      "Dataset.path"=NA,
      "Dataset.format"=NA,
      "Dataset.type"=NA)

  write.csv(
    datasets,
    template.filename,
    na="",
    quote=FALSE,
    row.names=FALSE)

  cat(paste("Import Template written to ", template.filename, ".\n", sep=""))

  readme <-
'
Readme for datasets template
============================

"Dataset.name": Name of the dataset. Must be unique within a study.

"Dataset.path": Path to the dataset.

"Dataset.format: Format of the file:
* Stata
* SPSS
* CSV

Dataset.type:
* "cross": Cross-sectional dataset.
* "long":  Dataset in the long-format.
* "spell": Spell dataset.
'

  write(readme, readme.filename)

  cat(paste("Readme file written to ", readme.filename, ".\n", sep=""))

}



