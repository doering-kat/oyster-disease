The oyster-environment-disease system in Chesapeake Bay, MD
================

Project Description
-------------------

The purpose of this project was to investigate the relationships between environmental conditions (captured by temperature and salinity), disease level (captured using prevalence), and oyster natural mortality rate and how these relationships may have changed over time. We found evidence that the relationships between disease level and natural mortality may have changed over time, which could indicate that oysters are developing resistance to MSX and potentially dermo.

This work was originally part of Kathryn Doering's master thesis.

Organization
------------

### Code

The code necessary to run the analyses are included in the `Code` directory. To run these scripts in the order they were created:

1.  Interpolate\_Salinity: Scripts used to interpolate the salinity and temperature data using Kriging and pull predictions at oyster bars of interest.
2.  Filter\_Kriging: Remove unreasonable predictions from the salinity and temperature kriging predictions.
3.  Summarize\_Disease: Manipulate the disease data to prepare it for use in structural equation modeling.
4.  SEM: Original structural equation modeling (SEM) analyses
5.  Create\_Figures: Scripts to make figures plotting results of the original SEM.
6.  PostThesis\_SEM: Analyses done after KD's thesis to address limitations of the original structural equation modeling approach.
7.  Explorations: These scripts were created at various times thoughout the project to explore small aspects, but do not contain the bulk of the work.

### Results

![Figure 1. Coefficients](Figures/PostThesis_SEM/4_Plot_Base_Results/coefficients_ts_2_spat_2.png)
