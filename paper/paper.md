---
title: 'histoc: Histocompatibility analysis performed by kidney allocation systems'
tags:
- R
- transplantation
- kidney
- allocation
date: "24 July 2022"
output: pdf_document
authors:
- name: Bruno A Lima
  orcid: 0000-0001-9090-4457
  equal-contrib: yes
  affiliation: 1
- name: Filipe P Reis
  orcid: 0000-0002-8186-3910
  equal-contrib: yes
  affiliation: 1
bibliography: paper.bib
link-citations: yes
affiliations:
- name: Oficina de Bioestatistica, Portugal
  index: 1
---

# Summary

The distribution of a scarce commodity such as deceased donor’s kidneys for transplantation should be as equitably as possible. Different countries try to implement kidney allocation systems (KAS) in transplantation that balance principles of justice and utility in the distribution of such scarce resource [@Lima:2020]. That is, a KAS should optimize the transplant clinical outcome (principle of utility) while giving a reasonable opportunity to all wait list candidates to be transplanted (principle of justice) [@Geddes:2005].

The selection of a donor-recipient pair in kidney transplantation is based on histocompatibility tests that can eliminate specific transplant candidates from opting for a kidney from a given deceased donor. These histocompatibility tests are used in several KAS and can be specific to each of them.

The goal of this package is to aid the evaluation and assessment of KAS in transplantation.

# Statement of need

{histoc} [@histoc] is an R [@R] package that assembles tools for histocompatibility testing in the context of kidney transplantation. The package's main functions allow simulating several KAS on the distribution of deceased donors’ grafts for transplantation. Moreover, it is possible to redefine arguments for each one of the KAS as a way to test different approaches.

Currently, it is possible to simulate allocation rules implemented in Portugal (PT model), in countries within Eurotransplant (ET model) [@ET], in the United Kingdom (UK model) [@UK], and a system suggested by @Lima:2013 (Lima’s model).

Each one of these models have as arguments a data frame with transplant candidates’ clinical and demographic characteristics, a data frame with candidates’ HLA antibodies and potential donor's information.

For all the models a virtual crossmatch between the donor and transplant candidates is performed (`xmatch()`). And, only those candidates with a negative crossmatch and ABO compatible can opt to a donor’s kidney.

Results are presented as {data.table} [@data.table] objects due to its high computation performance.

To get started a vignette describes [how to use](https://txopen.github.io/histoc/articles/how_to.html) each of the algorithms.

By default 2 candidates are selected for each donor, although we can define the number of candidates to be selected.

## Kidney Allocation Systems

### Portuguese Model

Portuguese rules on allocation of kidneys from deceased donor (**PT model**) are based on a scoring system that takes in consideration:  
1. HLA mismatches between donor and transplant candidate; 
1. Level of immunization of the candidate; 
1. Time on dialysis;
1. Age difference between donor and transplant candidate (`pts()`). 

Total scores for donor-recipient pairs are given by the column `ptsPT`. Also, hipersensitized candidates (`hiper()`) ( calculated Panel Reactive Antibody `cPRA` > 85%) `HI` are prioritized and after that
all candidates are ordered by their corresponding score.

### Euro Transplant Model

A simplified version of EuroTransplant Kidney Allocation System (**ETKAS**) [@ET] can be simulated through `et()`. This applies to first time kidney only candidates with more than 18 years old and that haven't donated one of their own kidneys.  

In this simulation for each donor, transplant candidates as sorted as :

1. Senior Program (65+ years of old candidates when the donor has 65+ years) `SP`;
1. Acceptable Mismatch Program (candidates with a cPRA > 85% and without HLA antibodies against HLA's donor) `AM`;
1. 000 HLA mismatches (candidates without HLA mismatches with the donor) `mmHLA`;
1. ETKAS points `pointsET`.

Final points for each eligible candidate are obtained from the sum of HLA points (`et_mmHLA()`), dialysis (`et_dialysis()`) points and MMP points (`et_mmp()`).

### United Kingdom Model

United Kingdom (**UK model**) deceased donor kidney allocation for transplantation [@UK] (`uk()`) is firstly based on the definition of two ranked Tiers of candidates eligible for the donor (`Tier`):

1. Tier A – patients with match score = 10 or `cPRA` = 100% or time on `dialysis` > 7 years;
1. Tier B – all other eligible patients.

Within Tier A, transplant candidates are ordered by matchability and time on dialysis. Transplant candidates within Tier B are prioritized according to a point-based system computed with 7 elements:

1. Matchability (`matchability`)
1. Time on dialysis (`dialysis`)
1. Donor-recipient risk index combinations (`ric()`)
1. HLA match and age combined (`age` and `mmHLA`)
1. Donor-recipient age difference (`age` and `donor_age`)
1. Total HLA mismatch (`mmHLa`)
1. Blood group match (`abo_uk()`)

This function simulates the allocation of kidneys to a candidates' waiting list for kidney-only transplants and do not take in consideration geographical criteria.

### Lima's Model

And lastly, within **Lima's model**, a color prioritization (`cp()`) of all waiting list transplant candidates is established.

Transplant candidates are classified according to their clinical urgency (red color), and regarding their time on `dialysis` and cPRA value `cPRA`. With an orange color are marked those patients with a `cPRA` > 85% or with a time on `dialysis` higher than waiting time 3rd quartile. As yellow are classified the patients with a `cPRA` > 50% or with a time on `dialysis` higher than waiting time median. And, as green are classified all the rest.

Within each color group candidates are ordered by `mmHLA` (ascendant) and time on `dialisys` (descendant).

Also, candidates are allocated to donors within the same age group (old to old program) (`SP`), mimicking EuroTransplant senior program (`sp()`).

## Candidates’ selection for a pool of donors

We can also simulate the selection of wait list candidates for a pool of donors, according to a given model (or algorithm). 
The function `donor_recipient_pairs()` allow us to compute all possible donor-recipient pairs according to any of the previously described kidney allocation algorithms. 

An example where we use a pool of 70 donors (data frame available from the package) to select from a wait list of 500 transplant candidates (data frame also available from the package) is described by [candidates' selection](https://txopen.github.io/histoc/articles/cand_select.html) vignette.


Also a new column for the estimated 5-year event (mortality or graft failure) probability as described by @Molnar:2018] and available from the application [TxScore](https://balima.shinyapps.io/scoreTx/), with the function `txscore()`, can be computed.

## Input data

Input data used on this package's functions, regarding either candidates or donors information, when provided by the user must have the exact same format as the example data available. Furthermore, [{simK}](https://github.com/txopen/simK) [@simK] package allows to generate synthetic data both for candidates and donors that can be used with {histoc}.  

# Funding

This project received the “Antonio Morais Sarmento” research grant from the Portuguese Society of Transplantation. This funding had no role in: study design; software development; the writing of the report; neither in the decision to submit the article for publication.

# References
