# Group_H_Lab02 - Puma Marketing Strategy in Ōtautahi (Parkrun)

DATA201/422 – Group H - Lab 02  
Last updated: 2025-10-10

This repository presents a technical analysis of parkrun data from seven Christchurch locations to help Puma spot
the best marketing opportunities, understand how runners behave, and better target potential customers.

---

## Purpose

Help Puma decide on marketing strategies by answering:

1. *Which Parkrun provides the best marketing opportunity with the largest number of people?*
2. *Do new people come each week or is it always the same people?*
3. *Do people move between Parkruns? If you advertise at fewer places, do you eventually see people from other locations?*
4. *If Puma want to target elite runners, where should they focus their efforts?*
5. *Puma believe more consistent runners have better potential as customers — how many are consistent, and is consistency different across locations?*
6. *Does the distance between Parkrun locations predict the amount of mixing of people between those locations?*

---

## Repository Structure

Group_H_Lab02/
├─ data/
│  ├─ input                         # raw files
│  ├─ out                          # cleaned, zipped final_df.csv 
├─ plots/                         #output plots used in stakeholder reports 
├─ scripts/
│  ├─ 01_data_cleaning.R
│  ├─ 02_data_analysis.R
├─ Trello                   #screenshots of the trello dashboard 
├─ Reports/
│  ├─ Technical_report
│  ├─ Stakeholders_report
├─ Group_H_Lab_02     #Rstudio Project File
└─ .gitignore

---
## Requirements

- *R ≥ 4.2* (tested on recent R)

Install once:
r
install.packages(c(
  "dplyr","tidyr","ggplot2", "tdiyverse","stringr","hms","lubridate","ggrepel","geosphere",
   "leaflet", "leaflet.extras", "leaflet.providers", "kableExtra","scales", "mapview","htmlwidgets", "patchwork"
))

---

## How to Run?

From repo root:

bash
Rscript scripts/01_data_cleaning.R
Rscript scripts/02_data_analysis.R

Each script prints a console recap and writes CSV/TIFF outputs to out.

---

## Links

- *Trello board:* 
https://trello.com/b/U5qytuWo/data422-data201-group-project
- *Git Repository*
https://github.com/DATA422-DATA201-25S2/Group_H_Lab_02
---
