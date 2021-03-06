% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genomics.df.long.R
\name{genomics.df.long}
\alias{genomics.df.long}
\title{Create a data frame with genomic alterations listed in long format}
\usage{
genomics.df.long(data)
}
\arguments{
\item{data}{tibble or data frame}
}
\value{
A tibble containing record_id, lesion_tag_genomics, genomics_tissue_type, gene, nucleotide, protein, cnv_yn with the gene arranged in long format
}
\description{
`genomics.df.long()` wrangles and processes genomics data from a REDCap project that has incorporated the Genomics Instrument. This allows for expedited analysis of patient-level data from REDCap. Genetic alterations are listed in long format.
}
\examples{
# Create a new DF with sample data set
BodyMap::mock_mcc_dataset \%>\%
  genomics.df.long()

}
