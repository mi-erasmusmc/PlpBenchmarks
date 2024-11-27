PLP Benchmark
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Patient-Level Prediction**
- Study type: **Methods Research*
- Tags: ****
- Study lead: **Ross D. Williams**, **Solomon Ioannou**
- Study lead forums tag: **[[RossW]](https://forums.ohdsi.org/u/rossw)** **[[SolomonI]](https://forums.ohdsi.org/u/solioa)**
- Study start date: **-**
- Study end date: **-**
- Protocol: [Find the protocol here](../blob/master/docs/Protocol/Protool.html)
- Publications: **-**
- Results explorer: **-**

This repo contains the software to run the PLP Benchmarking problems.

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.5
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 100 GB of free disk space
- OMOP CDM database
- Java for the JDBC connection
- R (plus R studio is recommended)
- The R package keyring to be set up

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java.

2. Open your study package in RStudio. Use the following code to install all the dependencies:

	```r
	install.packages("renv")
	renv::activate()
	renv::restore()
	```

3. In RStudio, select 'Build' then 'Install and Restart' to install the `PLPBenchmarks` package.

4. Before running the analysis, fill in the missing parts of the code provided under `extras/CodeToRun.R`. Then run the contents of the to run the analysis. 
