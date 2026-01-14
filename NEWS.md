
## Version 0.2.0
- Bugfix: Fix dilution node selection
- Bugfix: Fix decimals in dilution schema
- Remove strict assertions on MQC and HQC. Only warning is raised. 
- Bugfix: initialize cache path if not exist
- Remove inlet method file. Create method database to be connect to chromatography work
- Bugfix: remove rappsdir to pass CRAN checks
- New layout for compound ratios in the sequence. 

## Version 0.3.0
- Plates now can have different filling methods; either vertical or horizontal with bounds on both directions.
- Add well coloring by type.
- add_samples() can either propagate time or act as vectorized function. 

## Version 0.4.0
- Import Waters raw data files and plot chromatograms.
- Allow replications for samples.
- 
## Version 0.5.0
- Bioanalytical `group` adding allowing more systematic injection sequence creation. 
- Support for plate generation from `study_app` GUI 
- plate generation undo support 
- tree visualization for plate
- Support for analytical blanks
- Support for study registration
- Injection sequence ending now has specific pattern of letter and number, for instance A2 means location A and repeated 2nd time.
- Enhanced naming convention for reinjections and replicates.

 