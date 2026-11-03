# TARGET-EU CASE STUDY 3

## Objective
The primary objective is to estimate the effect of initiating dapagliflozin versus a DPP-4 inhibitor on time to first MACE, defined as a composite of non-fatal myocardial infarction, non-fatal stroke, and all-cause mortality among patients who have or are at risk of atherosclerotic cardiovascular disease (ASCVD)

## Background
Dapagliflozin, a sodium-glucose co-transporter 2 (SGLT2) inhibitor, has demonstrated cardiovascular benefits in randomized trials, including reduced hospitalization for heart failure and renal disease progression. Real-world studies have shown that SGLT2 inhibitors consistently reduce hospitalization for heart failure, with some evidence of mortality benefits, but their effects on major atherosclerotic cardiovascular events (MI and stroke) are less consistent.

## Protocol

[Published version](https://solisservices.sharepoint.com/:w:/r/sites/target-eu_roc19/Shared%20Documents/General/02_Protocol/WP3/Case%20study%203%20Declare-TIMI%2058%20trial/Protocol%20with%20Revisions%20after%20catalogue%20publication/Emulation_Protocol_CS3_REV5_clean.docx?d=w5d7db21e35414ebea35c47a61182e962&csf=1&web=1&e=8nV8fA)

[Track Changes version](https://solisservices.sharepoint.com/:w:/r/sites/target-eu_roc19/Shared%20Documents/General/02_Protocol/WP3/Case%20study%203%20Declare-TIMI%2058%20trial/Protocol%20with%20Revisions%20after%20catalogue%20publication/Emulation_Protocol_CS3_REV5_TC.docx?d=w6dab9115030640d495973054d9f1d13e&csf=1&web=1&e=jgdnDo)

[Shell tables and figures](https://solisservices.sharepoint.com/:w:/r/sites/target-eu_roc19/Shared%20Documents/General/02_Protocol/WP3/Case%20study%203%20Declare-TIMI%2058%20trial/Shell%20tables%20CS%203/Shell%20Tables%20CS%203%2026%20March%202026.docx?d=w9669f8889ab44e148116cf42728f8154&csf=1&web=1&e=z83egd)

## Repository Structure
```
[case-study-03-dapagliflozin]
|____configuration
|____data
| |____D2_cdm
| |____D3_study_variables
| |____D4_analytic_datasets
| |____D5_results
| |____D6_report
|____docs
|____logs
|____run
|____tests
|____transformations
| |____T2_semantic_harmonization
| |____T3_study_design
| |____T4_statistical_analysis
| |____T5_processing_results
|____CHANGELOG.md
|____LICENSE
|____README.md
```

## Data Overview
- Data follow the Common Data Model (CDM) structure.
- Data remain on secure servers; only dummy data is included here if it does not exceed the threshold on GitHub.
- Dummy data is used for testing and development.

## How to Run
Run the full pipeline with:

```
run/run_pipeline.R
```
Or run specific steps, e.g.:

```
run/run_T2.R
```

## Testing
Run tests with:

```
testthat::test_dir("tests")
```

## Contributing
- Please open an issue before starting a major change.
- Use feature branches and submit pull requests for review.
- Follow coding guidelines, see [coding-practices](https://github.com/target-roc19/coding-practices)

## License
XXX

## Contact
For questions or support, contact:
- Case Study Leads
    - Name: Daniala weir
    - Email: d.l.weir@uu.nl
- Programming Lead
    - Organisation: ARS Toscana 
    - Email: XXXXX
