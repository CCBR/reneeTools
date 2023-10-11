## code to prepare `wt_s1_genes` dataset goes here

# WT_S1.RSEM.genes.results was generated from running RENEE v2.5.3 on the test dataset https://github.com/CCBR/RENEE/tree/e08f7db6c6e638cfd330caa182f64665d2ef37fa/.tests
wt_s1_genes <- readr::read_tsv("data-raw/WT_S1.RSEM.genes.results") %>%
  filter(expected_count > 0) %>%
  .[1:10, ] # system.file('data-raw', 'WT_S1.RSEM.genes.results'))
usethis::use_data(wt_s1_genes, overwrite = TRUE)
