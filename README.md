# TARGET-EU CASE STUDY 3

This repository contains code which implements [case study 3](https://catalogues.ema.europa.eu/node/4883/administrative-details) of the [TARGET-EU project](https://catalogues.ema.europa.eu/node/4440/administrative-details): Comparative effectiveness and safety studies using the target trial emulation and estimand frameworks. The repository contains the code which implements the study design of this case study, along with documentation of that code, and where feasible, dummy data which allows users to run and understand the code. Below we describe briefly the case study and document the repository and its operation.

## Objective
The primary objective is to estimate the effect of initiating dapagliflozin versus a DPP-4 inhibitor on time to first MACE, defined as a composite of non-fatal myocardial infarction, non-fatal stroke, and all-cause mortality among patients who have or are at risk of atherosclerotic cardiovascular disease (ASCVD)

Dapagliflozin, a sodium-glucose co-transporter 2 (SGLT2) inhibitor, has demonstrated cardiovascular benefits in randomized trials, including reduced hospitalization for heart failure and renal disease progression. Real-world studies have shown that SGLT2 inhibitors consistently reduce hospitalization for heart failure, with some evidence of mortality benefits, but their effects on major atherosclerotic cardiovascular events (MI and stroke) are less consistent.

## EMA Catalogue Entries

[Entry of Case Study 3](https://catalogues.ema.europa.eu/node/4883/administrative-details)

[Protocol](https://catalogues.ema.europa.eu/system/files/2026-05/Emulation_Protocol_CS3_REV5_clean.pdf)

## Repository

This repository contains all code necessary to reproduce the results contained in the case study report, that is, the analytic pipeline designed to implement the specific study design. The repository does not contain any real data: the real data remains on secure servers, and only fictitious dummy data is shared here for testing, development and illustration purposes.

The analytic pipeline is conceptualised as a series of transformations (denoted "T") which transform data objects ("D") to other data objects [Gini et al. 2016](https://pubmed.ncbi.nlm.nih.gov/27014709/), reprsented at high-level by this image

<img width="1538" height="767" alt="image" src="https://github.com/user-attachments/assets/ae1cd558-c996-4289-943c-15ec2af64144" />

The exact analytic pipeline for this study is represented as a graph containing specific data objects and transformation and their relationships, available at this link

Each data object in the pipeline is documented in a corresponding codebook which describes how the data object is obtained from its inputs. Thereby, the codebooks specify the implementation of the given study design, which is executed by the scripts in the repository.

### Repository Structure
```
[case-study-03-dapagliflozin]
|____data
| |____D2_cdm
|____run
|____tests
|____transformations
|____CHANGELOG.md
|____LICENSE
|____README.md
```

### Structure Overview
- Data in D2_cdm contains the raw input data, following a Common Data Model (CDM) structure, in this case the [ConcePTION CDM](https://pmc.ncbi.nlm.nih.gov/articles/PMC9299060/).
- The transformations folder contains the scripts which compose the analytic pipeline
- During execution, intermediate data objects created by transformations are stored in the data folder.
- The test folder contains data for unit testing (see below)

### How to Run

1. Clone the repository on your computer
2. Open the file run/to_run.R
3. The file contains a header, including the parameter TEST, few configuration files, and a list of steps
4. The parameter TEST that can be set to TRUE or FALSE
   - if FALSE: the steps can be run sequentially from the dummy CDM in data/D2_cdm down to tables and figures
   - if TRUE: some steps have unit test data that allow to inspect specific behaviours on explicit datasets

## License

See [Licence](https://github.com/target-roc19/case-study-03-dapagliflozin-public/blob/main/LICENSE.txt)

## Open science badges

![Open Code](https://img.shields.io/badge/Open%20Code-yes-brightgreen)

## Contact
For questions or support, contact:
- Case Study Leads
    - Name: Daniala weir
    - Email: d.l.weir@uu.nl
- Programming Lead
    - Organisation: ARS Toscana 
    - Email: rosa.gini@ars.toscana.it
