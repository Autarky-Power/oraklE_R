## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

❯ checking installed package size ... NOTE
    installed size is 14.7Mb
    sub-directories of 1Mb or more:
      data   9.1Mb
      doc    5.3Mb

historic_entsodata1.Rda , historic_entsodata2.Rda and weo_data.Rda are necessary for the library and can't be compressed further. 
The remaining data files are for documentation purposes and are necessary to run the respective examples.
Regarding the doc folder, I included a lot of graphics in the vignette. I could remove them but i think keeping them, really helps understanding 
what the library does.      
