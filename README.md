# The Effect of E-Invoicing on Tax Revenue and Compliance in El Salvador
Grant-funded through the William & Mary Charles Center Summer Research Grant, this faculty-advised study (Advisor: Prof. Andrew Dustan) analyzes El Salvador’s staggered e-invoicing rollout (2023–2025) using Callaway & Sant’Anna (2021) DiD.

**Headline result:** Early evidence shows no significant VAT revenue gains among larger firms, who already had high compliance and digital adoption. Estimates are presented with event-study confidence bands.
![Main Figure](data/analysis/attrevenue.png)

## Quick start
1) Install R >= 4.4.1 and RStudio
2) Install required packages:
	‘’’ r
	install.packages(c(“tidyverse”, “lubridate”, “scales”, “here”, “zoo”, “did”, “patchwork”, “broom”))
3) Place raw VAT data in data/raw/
4) Run the full analysis: source(“el_salvador_einvoicing_main.R”)

## Data
- **Source:** Ministerio de Hacienda, El Salvador Fiscal Transparency Portal (VAT administrative data).
- To replicate results, users must download the datasets directly from the Ministry of Finance and place them in `data/raw/`.
- See `data/raw/README.md` for more.

## Methods
Event-study DiD (Callaway & Sant’Anna 2021), robust SEs, staggered adoption by taxpayer cohort.

## Files
/code — script | /data — download information & graphical output | /paper — PDF + slides

## Cite  
Lederman, Adam (2025). *The Effect of E-Invoicing on Tax Revenue and Compliance in El Salvador*. Undergraduate Research Project, College of William & Mary.  
[GitHub Repository](https://github.com/aelederman/el-salvador-einvoicing)  

*(A DOI will be added if archived on Zenodo in the future.)*

Contact: aelederman@wm.edu  
License: MIT
