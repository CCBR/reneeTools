# WT_S1.RSEM.genes.results was generated from running RENEE v2.5.3 on the test dataset https://github.com/CCBR/RENEE/tree/e08f7db6c6e638cfd330caa182f64665d2ef37fa/.tests
gene_counts <- readr::read_tsv("data-raw/RSEM.genes.expected_count.all_samples.txt")
usethis::use_data(gene_counts, overwrite = TRUE)
