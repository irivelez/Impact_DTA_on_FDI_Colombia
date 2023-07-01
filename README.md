# Problem Set Template Repository for Big Data and Machine Learning for Applied Economics

This is the template repository for the problem sets.

The repo should contain at least four folders:

- `document`: contains your final document in `pdf` format. Ideally, the document should pull figures and tables directly from the `views` folder. I've included a latex template I created for the Thesis Seminar. 
- `scripts`: contains all your scripts
- `stores`: contains all the data sets used. If files are "too big" for GitHub, include a document describing where people can access the data.
- `views`: contains all figures and tables


## Some general instructions 

Instructions

The database used for the entire workshop is available in the stores folder of the repository and is called data.Rdata.

To replicate the work in Rmarkdown it is not necessary to run the lines of code corresponding to the Import_data and Transforming_data chunks, since they process the information directly from the pages and perform the data transformation; instead, the Import_DB chunk can be used to import the transformed database.

Additionally, it is important to note that point 5 will take time as it must iterate the 2 models for all observations. However, to facilitate this, the base available in stores should be loaded.