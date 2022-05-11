
## Import
### Usage

There are two ways for importing cohorts:
1) via ATLAS
2) via JSONs

#### Import via ATLAS

This tab is used to import data from your OMOP CDM database. You can import all cohorts defined in ATLAS (also generated). You can see a selection of numbers (ids) for "Choose the target cohort" and "Choose the state cohorts". These ids represent cohorts you have defined in ATLAS's interface under "Cohort Definitions" tab.

**Choose the target cohort:** Defined and generated cohort which is handled as the population under investigation.  
**Choose the state cohorts:** Defined and generated cohorts which are handled as states of transfer (subsets of the patient cohort). These transfers are analysed and described with the Cohort2Trajectory tool.

After selecting the cohorts of interest hit the "**Import cohorts**" button.

After importing cohorts some statistics will be shown under "Statistics" tab. Use these for tuning your cohorts and confirming that you imported the right cohorts.

**State overlap heatmap:**

Use this functionality after importing the data. This heatmap shows, how much do the states defined by you overlap. If the percentage of overlapping is high, we suggest that those states should be combined or defined more precisely. 

**Requirements**
The Database user using this tool has to have READ rights in OMOP CDM schema as well as results schemas. The tool also creates some temporary tables, therefore the user should also have temp table creation rights in the specified (cdmTmpSchema) schema. 

##### Customize

This tab is used to denote the imported cohorts. Choose a name for each of your cohorts. When customizing your state cohorts please separate the names by commas.

**Name the selected patient cohort:** Denote the population cohort.

**Name the selected state cohorts:** Denote the imported state cohorts in the order of importing, separate by commas.

**Prioritize your state cohorts by Ids** Create a sequence of priority (descending). Under the "Prioritising" tab you can "drop-and-drag" the state cohort labels to customise your preferred prioritized state sequence.


