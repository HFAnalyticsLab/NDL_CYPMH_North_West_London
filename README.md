<img src="ndlbanner.png" width="405" height="96">

# Networked Data Lab: NDL North West London
## Impact of the COVID-19 pandemic on the mental health of the children and young people population in North West London

#### Project Status: In-progress

## Project Description

The Networked Data Lab (NDL) is a pioneering collaborative network of analysts who use linked data, open
analytics, and public and patient involvement to tackle the most pressing challenges in health and social
care. The initiative is led by The Health Foundation, working closely with five partner labs across the UK.
Our lab, North West London (NWL), is a partnership between Imperial College Health Partners (ICHP), NWL
Health and Care Partnership and the Institute of Global Health Innovation (IGHI).
This report is the second in the NDL’s programme and will explore how the children and young people’s
(CYP) population’s access to mental health (MH) services was disrupted by the COVID-19 pandemic.
We know, for example, that the pandemic has put a large and unforeseen strain on healthcare services
(Tangcharoensathien et al. 2021; Mahase 2021), and on people’s MH (Nearchou et al. 2020; Ford, John,
and Gunnell 2021).

Our analysis has been undertaken through a collaborative approach with professional partners in NWL,
public and patient involvement and using data from NWL’s depersonalized Discover dataset.
In order for us to better determine what research questions are more important to NWL CYP, we conducted
a patient and public involvement and engagement (PPIE) partnership with a Young People’s Advisory Group
(YPAG) to determine their priorities. The YPAG was a diverse group of 20 young people from NWL (e.g. 65%
from ethnic minority groups and 20% non-binary). This was followed by a prioritisation exercise in which
CYP that were consulted with, outlined a list of research questions and priorities, that we considered could be
further investigated using the Discover dataset. We also consulted a group of multidisciplinary, healthcare
professionals in NWL working on the delivery of MH services that forms our professional reference group
(PRG) for their research priorities.

Finally, the NWL lab agreed to investigate three main topic areas:
- Access to MH services
- Severity of MH difficulties
- Transitions to other MH services.


Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data sources

This analysis used the following data:

- [Discover](https://www.discover-now.co.uk)
- [ONS Clinical commissioning group population estimates](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates)

## Requirements

These scripts were written in R version 3.6.1 within the Discover research environment. The following R packages were used:

- readxl (v 1.3.1)
- dplyr (v 1.0.7)
- tidyverser (v 1.2.1)
- data.table (v 1.14.0)
- readr (v 1.4.0)
- magritrr (v 2.0.1)
- lubridate (v 1.7.10)
- gridExtra (v 2.3)
- zoo (v 1.8-9)

## Getting started

You can find the R code used to process the data used in this report on the 'Codes' directory. Here you can find three scrips:
- NDL2_analysis_V2.R: This script was used to calculate values for the objectives 1 (Describe the CYP MH population in NWL), 2 (Assess the impact of COVID-19 and the CYP MH service use) and 3 (Investigate the impact of COVID-19 on the severity of MH difficulties)
- NDL2_analysis_V2.R: This script contains the R code used to analyse data shown in our 4th analysis goal (Invstigate the impact of COVID-19 on trainsitions from CAMHS to adult MH services)
- denominator_calcs.R: This script was used to summarise the population data in the [ONS Clinical commissioning group population estimates](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates) dataset to standardise some of the values in our analysis of the CYP population accessing MH care

In this directory you can also find an excel file containing aggregate data used throughout this report, as well as codes used to identify MH appointments, conditions, medications and service users.

## Authors

- [Roberto Fernandez Crespo](roberto.fernandez-crespo1@imperial.ac.uk)
- Gulam Muktadir
- [Evgeniy Galimov](evgeny.galimov@imperialcollegehealthpartners.com)
- Sandeep Prashnar
- Sarah Houston
- Sadie Myhill
- [Clare McCrudden](clare@helixcentre.com)
- Emma Sharpe-Jones
- Lewis Thomas
- Tomazs Szymanski
- Alex Bottle
- Melanie Leis
- [Matthew Chisambi](matthew.chisambi@imperialcollegehealthpartners.com)

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).
