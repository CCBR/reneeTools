# ################################################################
# ##### standard
# ################################################################
## counts.R
- input: counts RSEM or f
- output: matrix of read counts, genes as rows and samples as cols

## filter_low_counts.R
- input: Robj @counts
- input: param for counts threshold
- output: Robj @counts

## metadata.R
- input: df with metadata
- output: Robj @metadata

# ################################################################
# ##### normalize
# ################################################################
## deseq2.R
- input: Robj @counts 
- input: Robj @metadata
- output: Robj @deseq2 (dds object)

## limma.R

## edgeR


# ################################################################
# ##### DEG
# ################################################################
## deg.R
- input: Robj @deseq2
- input: param padj
- input: param log2fc
- output: Robj deg results

# ################################################################
# ##### plots
# ################################################################
## pcatools.R
- input: Robj @deseq2
- input: Robj @metadata
- input: param list of comparisons variables
- output: Robj gene list of PC1
- output: print pctools plot

## plot_RLE.R
- input: Robj @counts
- input: Robj @metadata
- input: param of counts_type (IE RAW, FILTERED)
- output: print RLE plot

## plot_PCA.R
- input: Robj @counts
- input: Robj @metadata
- input: param of counts_type (IE RAW, FILTERED)
- output: print PCA plot

## plot_volcano.R
- input: Robj @deseq2
- input: Robj @counts FILTERED
- input: param padj
- input: param log2fc
- output: print volcano plot

# ################################################################
# #### MISC
# ################################################################
## reexports.R
- exports %>%
- exports color_list

# ################################################################
# #### OBJ
# ################################################################
## renee-class.R
- sets up the obj structure

## reneeTools-package.R
- helper functions

# ################################################################
# #### to work on 
# ################################################################
## data.R
- unclear what this is supposed to do?
