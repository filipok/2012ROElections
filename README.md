Some background on the 2012 Romanian elections 
================================

Local elections, June 2012. Details: https://ro.wikipedia.org/wiki/Alegeri_locale_%C3%AEn_Rom%C3%A2nia,_2012

Impeachment referendum, xxx 2012. Details:

General (Parliament) elections (two chambers), xxx 2012. Details:


Data sources
================================
*Later*

Download and load the data
================================

To get the source data, run **getData.R**. Data is hosted on Dropbox, see the code for details. If you don't feel like re-running most of the pre-processing phase, then use **loadDump.R** (which was created by running **writeDump.R**). It will load the required objects into the memory.

**getData.R** requires **getGenElePrecinct.R** and **generalFunctions.R**.


Use the data (after the previous step)
================================

For **local elections**, load **localElections.R**. It requires **localFunctions.R**.

For **general (Parliament) elections**, run **parlElections.R**. It requires **parlFunctions.R**.

For the **referendum**, run **refElections**.

Work in progress
---------------------------------

**precComb...** files -> combining local, referendum and precinct data.

**2012-work-locale....** -> currently does not work.