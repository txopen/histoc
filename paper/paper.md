---
title: 'histoc: Histocompatibility analysis performed by kidney allocation systems'
tags:
- R
- transplantation
- kidney
- allocation
date: "24 July 2022"
output:
  html_document:
    df_print: paged
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
- name: Oficina de Bioestatistica, Ermesinde, Portugal
  index: 1
---

# Summary

The distribution of a scarce commodity such as deceased donor’s kidneys for transplantation should be as equitable as possible. Different countries try to implement kidney allocation systems (KAS) in transplantation that balance principles of justice and utility in the distribution of such scarce resource [@Lima:2020]. That is, a KAS should optimize the transplant clinical outcome (principle of utility) while giving a reasonable opportunity to all wait list candidates to be transplanted (principle of justice) [@Geddes:2005].

The selection of a donor-recipient pair in kidney transplantation is based on histocompatibility tests that can eliminate specific transplant candidates from opting for a kidney from a given deceased donor. These histocompatibility tests are used in several KAS and can be specific to each of them.

The goal of this package is to aid the evaluation and assessment of KAS in transplantation.

# Statement of need

`histoc` is an R [@R] package that assembles tools for histocompatibility testing in the context of kidney transplantation. The package's main functions allow simulating several KAS on the distribution of deceased donors’ grafts for transplantation. Moreover, it is possible to redefine arguments for each one of the KAS as a way to test different approaches.

Currently, it is possible to simulate allocation rules implemented in Portugal [PT model; @PT], in countries within Eurotransplant [ET model; @ET], in the United Kingdom [UK model; @UK], and a system suggested by [@Lima:2013; Lima’s model].

Each one of these models have as arguments a data frame with transplant candidates’ clinical and demographic characteristics, a data frame with candidates’ Human Leukocyte Antigens (HLA) antibodies and potential donor's information. By default two candidates are selected for each donor, although we can define the number of candidates to be selected.

For all the models a virtual crossmatch between the donor and transplant candidates is performed (`xmatch()`). And, only those candidates with a negative crossmatch and ABO compatible can opt to a donor’s kidney.

Results are presented as `data.table` [@data.table] objects due to its high computation performance.

To get started, a vignette is available that describes [how to use](https://txopen.github.io/histoc/articles/how_to.html) each one of the algorithms.

New kidney allocation systems should be assessed using simulations that, to the greatest extent possible, can predict outcomes. `histoc` is designed mainly for researchers working on organ transplantation, assisting with data-driven decision making for the establishment of allocation policies.

While the R package `transplantr` [@transplantr] makes available a set of functions for audit and clinical research in transplantation, the package presented here enables the simulation of various sets of rules by adjusting relevant allocation parameters. Additionally, the Kidney-Pancreas Simulated Allocation Model [KPSAM; @srtr] is a proprietary software that the Scientific Registry of Transplant Recipients makes available to support studies on alternative allocations policies in transplantation. In contrast, `histoc`, coupled with being open source, needs less data to run in comparison to KPSAM software. Likewise, it can be used as a preliminary technique for developing new hypotheses that can then be tested on KPSAM.

## Kidney Allocation Systems

### Portuguese Model

Portuguese rules on allocation of kidneys from deceased donor [PT model; @PT] are based on a scoring system that takes in consideration:  

1. HLA mismatches between donor and transplant candidate;
2. Level of immunization of the candidate;
3. Time on dialysis;
4. Age difference between donor and transplant candidate (`pts()`).

Total scores for donor-recipient pairs are given by the column `ptsPT`. Also, hypersensitized candidates (`HI`; `hiper()`; calculated Panel Reactive Antibody `cPRA` > 85%) are prioritized and all subsequent candidates are ordered by their corresponding score.

### Euro Transplant Model

A simplified version of EuroTransplant Kidney Allocation System [ETKAS; @ET] can be simulated through `et()`. This applies to first time kidney only candidates with more than 18 years old and that haven't donated one of their own kidneys.  

In this simulation for each donor, transplant candidates as sorted as :

1. Senior Program (`SP`; 65+ years old candidates when the donor has 65+ years old);
2. Acceptable Mismatch Program (`AM`; candidates with a `cPRA` > 85% and without HLA antibodies against HLA's donor);
3. 000 HLA mismatches (`mmHLA`; candidates without HLA mismatches with the donor);
4. ETKAS points (`pointsET`).

Final points for each eligible candidate are obtained from the sum of HLA points (`et_mmHLA()`), dialysis (`et_dialysis()`) points and MMP points (`et_mmp()`).

### United Kingdom Model

United Kingdom deceased donor kidney allocation for transplantation [UK model; @UK] is firstly based on the definition of two ranked tiers of candidates eligible for the donor (`Tier`):

1. Tier A – patients with match score = 10 or `cPRA` = 100% or time on `dialysis` > 7 years;
2. Tier B – all other eligible patients.

Within Tier A, transplant candidates are ordered by matchability and time on dialysis. Transplant candidates within Tier B are prioritized according to a point-based system computed with 7 elements:

1. Matchability (`matchability`)
2. Time on dialysis (`dialysis`)
3. Donor-recipient risk index combinations (`ric()`)
4. HLA match and age combined (`age` and `mmHLA`)
5. Donor-recipient age difference (`age` and `donor_age`)
6. Total HLA mismatch (`mmHLa`)
7. Blood group match (`abo_uk()`)

The `uk()` function simulates the allocation of kidneys to a candidates' waiting list for kidney-only transplants and do not take in consideration geographical criteria.

### Lima's Model

And lastly, within Lima's model [@Lima:2013], a color prioritization (`cp()`) of all waiting list transplant candidates is established.

Transplant candidates are classified according to their clinical urgency (red color), with regard to their time on `dialysis` and cPRA value `cPRA`. Orange is used to mark patients with a `cPRA` > 85% or with a time on `dialysis` higher than the waiting time for the 3rd quartile. Yellow is used to classify patients with a `cPRA` > 50% or with a time on `dialysis` higher than waiting time median. Green is used to specify all other candidates.

Within each color group candidates are ordered by `mmHLA` (ascendant) and time on `dialysis` (descendant).

Also, candidates are allocated to donors within the same age group (`SP`; old to old program), mimicking EuroTransplant senior program (`sp()`).

## Candidates’ selection for a pool of donors

We can also simulate the selection of wait list candidates for a pool of donors, according to a given model (or algorithm). The function `donor_recipient_pairs()` allow us to compute all possible donor-recipient pairs according to any of the previously described kidney allocation algorithms.

We provide example datasets within `histoc` for 500 wait list transplant candidates and a pool of 70 donors both of which are described in our [candidates' selection vignette](https://txopen.github.io/histoc/articles/cand_select.html).

Moreover, an additional column in the output can be generated to calculate the estimated 5-year event probability for mortality or graft failure described by @Molnar:2018. This is available from the application [TxScore](https://balima.shinyapps.io/scoreTx/) and can be computed using the `txscore()` function.

## Input data

User provided input data used by this package regarding either candidate or donor information should match the exact format of the provided example data. Furthermore, the [`simK`](https://github.com/txopen/simK) [@simK] package allows users to generate synthetic data both for candidates and donors that can be used with `histoc`.  

## Bug reports and contributions

Any bug reporting, feature requests, or other feedback will be welcomed by [submitting an issue](https://github.com/txopen/histoc/issues) in our repository. When reporting a bug, please ensure that a reproducible example of your code is included so that we may respond to your issue promptly.

# Funding

This project received the “Antonio Morais Sarmento” research grant from the Portuguese Society of Transplantation. This funding had no role in: study design; software development; the writing of the report; neither in the decision to submit the article for publication.

# References
