
## Import
### Usage

This tab is used for data import.

#### Selected cohorts

This tab is used to import data from your OMOP CDM database. You can see a selection of numbers (ids) for "Choose the patient cohort" and "Choose the state cohorts". These ids represent cohorts you have defined in Atlas's interface under "Cohort Definitions" tab.

**Choose the patient cohort:** Defined and generated cohort which is handled as the population under investigation.  
**Choose the state cohorts:** Defined and generated cohorts which are handled as states of transfer (subsets of the patient cohort). These transfers are analysed and described with the Cohort2Trajectory tool.

After selecting the cohorts of interest hit the "**Import cohorts**" button.

After importing cohorts some statistics will be shown. Use these for tuning your cohorts and confirming that you imported the right cohorts.

**Requirements**
The Database user using this tool has to have READ rights in OMOP CDM schema as well as results schemas. The tool also creates some temporary tables, therefore the user should also have temp table creation rights in the specified (cdmTmpSchema) schema. 

#### State overlap heatmap

 Use this tab after importing the data. This heatmap shows, how much do the states defined by you overlap. If the percentage of overlapping is high, we suggest that those states should be combined or defined more precisely.  

#### Customise

This tab is used to denote the imported cohorts.

**Name the selected patient cohort:** Denote the population cohort.

**Name the selected state cohorts:** Denote the imported state cohorts in the order of importing, separate by commas.

**Prioritize your state cohorts by Ids** Create a sequence of priority (descending). Use the same notation as in the previous shell for states. This priority sequence will be used in case of some states overlapping in time.
