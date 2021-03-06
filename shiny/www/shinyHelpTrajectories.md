## Trajectories
### Usage

This tab is used for generating discrete and continuous patient treatment trajectory dataframes. For discrete trajectories generation means that the patients stay in a state for a fixed amount of time. The raw data (cohort inclusion datetimes) will be extracted to patient trajectories with each state having the timespan of the defined length. For example if a patient was in state \#1 for 3 months and our defined length of a state is 30 days, then the trajectory will have three sequential states of state \#1 of length of 30 days. 

#### Generation of trajectory dataframes (Discrete & continuous)

**Length of a state (days):** Enter the fixed length of the stay in a state. The initial value is given as 30 days (only relevant for discrete case).

**State selection type:** This option will define the state selection when states happen to overlap in patient treatment trajectories. 
  * First occurring: The state which occurs first in chronological order will be selected.
  * Largest overlap: The state which overlaps the most with the given timespan will be selected (only relevant for discrete case)
  * Priority first: The state which occurs in the timespan and is highest in the priority list will be selected.

**Choose absorbing states:** This option will define the  absorbing states in the trajectory. This means, that if the patient enters the selected state(s), the trajectory will be cut to end. Such states might be related to death or diseases from which there is no returning. Of course this feature might be used in any ways handy.

**Choose mandatory states:** This option will define the  mandatory states in the trajectory. This means, that if the patient does not have an occurrence of the selected state(s), the trajectory will be excluded from the result.

**Allow states out of observation period:** This option can allow patient trajectories to span out of the target cohort's observation period.  

After filling in the options, hit the **Generate** button to run the workflow. The trajectories will be saved to "~/tmp/datasets"

