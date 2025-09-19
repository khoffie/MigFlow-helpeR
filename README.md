This package contains the code to tidy the raw migration statistics,
as well as other needed data sets. It is only useful for MigFlow and
has no use outside the project.

The package does the following things:
 - calculate euclidean distances between point_on_surface (something
   like centroids but guaranteed to fall on the surface) of districts.
 - Adjusting flows from 2000-2017 to reflect the administrative
   district structure in 2017.
 - Swaps origin and destination since in the original data this is
   swapped.
 - For Berlin and Hamburg AGS in the data allows to identify certain
   areas. This information is removed and all are grouped to Hamburg
   or Berlin
 - Between raw files encoding of age group is not standardized (e.g.
   "65 und mehr" and "über 65" are both recoded to "above65")
 - In the early 2000s many resettlers came to Germany that were mostly
   registered in district Göttingen. These are removed from the
   analysis, since they only show up as outflows and this is really a
   different process.
 - Only moves of German citizens are analyzed, moves of Foreigners are
   removed.
 - A .csv of age group specific population of Germans in districts is
   created.
 -  
