## training data set from the NIDAP Bulk RNA-seq workflow

nidap_clean_raw_counts <- readr::read_csv(system.file(
  "extdata", "nidap", "Clean_Raw_Counts.csv",
  package = "reneeTools"
))
usethis::use_data(nidap_clean_raw_counts, overwrite = TRUE)

nidap_sample_metadata <- readr::read_csv(system.file(
  "extdata", "nidap",
  "Sample_Metadata_Bulk_RNA-seq_Training_Dataset_CCBR.csv",
  package = "reneeTools"
))
usethis::use_data(nidap_sample_metadata, overwrite = TRUE)

nidap_filtered_counts <- readr::read_csv(system.file(
  "extdata", "nidap",
  "Filtered_Counts.csv",
  package = "reneeTools"
))
usethis::use_data(nidap_filtered_counts, overwrite = TRUE)
