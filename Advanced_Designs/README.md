# Advanced design of experiments
### date: 02/12/2018
### Introduction:

Advanced design of experiments based on centered L2 discrepancy, Mixture L2 discrepancy and Wrap-around L2 discrepancy are uploaded to this repository. Both the old online best designs and new advanced designs are included and separated into different sub-directories. For Latin hypercube designs, the suffix is *_nsn, while for general designs the suffix is *_nsq. In each sub-directories, advanced designs after optimizating process by UniDOE are located in $Advance$ directory and their corresponding discrepancies are located in $Discrepancy$ directory.

The R files to obtain these advanced designs are also included in this repository.  

*_Improvement_ratio.txt files record the improvement ratio for the online best designs: http://www.cms-ud.com/UD/UniformDesign.html 

### Naming rules:

All .csv files follow naming rules : [crit_type]\_[run]\_[factor]\_[level].txt 

