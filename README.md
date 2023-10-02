# Unrestricted versus Regulated Open Data Governance: A Bibliometric Comparison of SARS-CoV-2 Nucleotide Sequence Databases 

## Authors: 

Nathanael Sheehan, Sabina Leonelli, Federico Botta

## Abstract: 

Two distinct modes of data governance have emerged in accessing and reusing viral data pertaining to COVID-19: an unrestricted model, espoused by data repositories part of the International Nucleotide Sequence Database Collaboration and a regulated model promoted by the Global Initiative on Sharing All Influenza data. In this paper, we focus on publications mentioning either infrastructure in the period between January 2020 and January 2023, thus capturing a period of acute response to the COVID-19 pandemic. Through a variety of bibliometric and network science methods, we compare the extent to which either data governance strategy facilitated collaboration from different countries around the globe to understand how data reuse can enhance forms of diversity between institutions, cities, countries, and funding groups. Our findings reveal disparities in representation and usage between the two data infrastructures. We conclude that both approaches offers useful lessons, with the fully open model offering insights into complex data linkage and the partially open model demonstrating the importance of global representation.  

## About
This repository contains the code and data used to produce the results presented in the paper " Unrestricted versus Regulated Open Data Governance: A Bibliometric Comparison of SARS-CoV-2 Nucleotide Sequence Databases".
 

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

All data used in the analysis can be found in the `data-raw` folder.

## Code

All code can be found in the `code/` folder. The folder contains eight `R` files and one `bash` file. The most techincal part of code is in the `code/utils.r` file, which contains documented functions used throughout the analysis. `code/packages.r` loads all the necessary packages for the project. The remaining files are as follows:
- two files are concerned with fetching/cleaning data (`code/data-wrangle.r`,`code/owid-data.r`) and 
- the rest (`code/regression.r`,`code/submission-plots.r`,`code/summary-stats.r`,`code/usage-network.r`,`code/architecture.sh`)

## Reproducing the results

The analysis of this work is full reproducible. However, before reproducing the results the reviewer must do two things after installing the repository:

(1) Navigate to `build.r.`
(2) change `reproducible = FALSE` to `TRUE`
(3) sit and watch the `imgs` folder populate with images from the paper

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
