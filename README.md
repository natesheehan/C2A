# From collection to analysis: A comparison of GISAID and the Covid-19 Data Portal

## Authors: 

[Nathanael Sheehan](link to author 1's website), [Sabina Leonelli](link to author 2's website), [Federico Botta](link to author 2's website)

## Abstract: 

We analyse ongoing efforts to share genomic data about SARS-COV-2 through a comparison of the characteristics of the Global Initiative on Sharing All Influenza Data and European Nucleotide Archive infrastructures with respect to the representativeness and governance of the research data therein. We focus on data and metadata on genetic sequences posted on the two infrastructures in the period between March 2020 and October 2022, thus capturing a period of acute response to the COVID-19 pandemic. Through a variety of data science methods, we compare the extent to which the two portals succeeded in attracting data submissions from different countries around the globe and look at the ways in which submission rates varied over time. We go on to analyse the structure and underlying architecture of the infrastructures, reviewing how they organise data access and use, the types of metadata and version tracking they provide. Finally, we explore usage patterns of each infrastructure based on publications that mention the data to understand how data reuse can facilitate forms of diversity between institutions, cities, countries, and funding groups. Our findings reveal disparities in representation between the two infrastructures and differing practices in data governance and architecture. We conclude that both infrastructures offer useful lessons, with GISAID demonstrating the importance of expanding data submissions and representation, while the COVID-19 data portal offers insights into how to enhance data usability.
About
This repository contains the code and data used to produce the results presented in the paper "Title of Paper" by [Author 1](link to author 1's website) and [Author 2](link to author 2's website), published in [Journal/Conference/Workshop](link to publication) in [Year of Publication].

## Usage

### Requirements
To run the code in this repository, you will need:

A computer: 

R: R is a free software environment for statistical computing and graphics. You can download the latest version of R from the R Project website.

RStudio: RStudio is an integrated development environment (IDE) for R that provides a user-friendly interface for coding and running R scripts. You can download the latest version of RStudio from the RStudio website. All additional packages are installed in the `packages.r` file.


### Installation

```{bash}
git clone https://github.com/natesheehan/C2A.git
cd C2a
```

## Data


| Data              | URL                                                               | Description                                                                                       |
| -----------------| ----------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| GISAID            | https://www.gisaid.org/                                            | A global science initiative providing open access to genomic data of influenza viruses and SARS-CoV-2|
| COVID-19 Data Portal | https://covid19dataportal.org/                                    | A global database of COVID-19 cases, deaths, hospitalizations, and other related genomic data              |
| Dimensions API    | https://docs.dimensions.ai/dsl/api.html                           | A database with various datasets, including COVID-19 research publications, patents, and grants   |
| Our World in Data COVID-19 Cases | https://ourworldindata.org/covid-cases                            | A dataset with daily COVID-19 cases for countries and regions                   |
| Our World in Data Income Groups | https://ourworldindata.org/global-economic-inequality              | A dataset with income group classifications for various countries and regions based on World Bank classifications                       |



## Code

All code can be found in the `code/` folder. The folder contains eight `R` files and one `bash` file. The most techincal part of code is in the `code/utils.r` file, which contains documented functions used throughout the analysis. `code/packages.r` loads all the necessary packages for the project. The remaining files are as follows:
- two files are concerned with fetching/cleaning data (`code/data-wrangle.r`,`code/owid-data.r`) and 
- the rest (`code/regression.r`,`code/submission-plots.r`,`code/summary-stats.r`,`code/usage-network.r`,`code/architecture.sh`)

## Reproducing the results

The analysis of this work is full reproducible. However, before reproducing the results the reviewer must do two things after installing the repository:

(1) first you must unzip the `/data-raw/submissions` folder
(2) change `reproducible = FALSE` to `TRUE` in `build.r`
(3) sign up to GISAID and download the Clade/Lineage Variants (tsv) file from EPICov.
(4) sit and watch, a full run should take around 10 minutes

## Citation

If you use the code or data from this repository in your research, please cite our paper:

```{sql}
@article{sheehan2023,
  title={From collection to analysis: A comparison of GISAID and the Covid-19 Data Portal},
  author={[Nathanael Sheehan,Sabina Leonelli,Federico Botta},
  journal={Journal},
  year={2023}
}
```

## License
MIT License

Copyright (c) 2023 Nathanael Sheehan

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Acknowledgments

This project has received funding from the European Research Council (ERC) under the European Union’s Horison 2020 research and innovation programme (grant agreement No. 101001145). This paper reflects only the author's view and that the Commission / Agency is not responsible for any use that may be made of the information it contains.
