# water-equity

## directory folder structure
1. 'code' contains all the simulation code necessary to 'perform runs' with subdirectories as needed
2. 'data' is for raw input data that we don't touch
3. 'analysis' is for scripts for making plots tables figures and probing code outputs, and storing small, clean outputs
4. 'documents' is for literature and things that we may write
5. 'outputs' is for big simulation outputs. ideally, we don't touch these. The simulation makes them, the analysis scripts read them.


Ideally the workflow looks like this:

For each city, we make a new folder in each of the 5 above just named 'cityname' ie 'sacramento'

* 'data/cityname' contains the raw data and any scritps which pre-process it and outputs from the pre-processing scripts
* 'code/cityname' is the model specifically updated based on that city. code here reads things from data and writes them to outputs
* 'outputs/cityname' is where all the code outputs go. We don't ever touch these, but there should be a README which describes how they are named
* 'analysis/cityname' is code which analyzes outputs and writes plots/outptus to other directorys in analysis. This is a little ad-hoc but that's because we have lots of different types of potential analyses to do. Anything in the root 'analysis' dir should be pulling from lots of cities. 

From my experience with this type of work, most re-doing (either in response to reviews, additional work etc...) comes from re-analzying simulation outputs. As such, we want to keep 'outputs' seperate and un-touched so we don't have to rerun all the models, we can just write new code in 'analysis' which makes plots, tables whatever ...

rachunok@stanford.edu 
08/13/21
