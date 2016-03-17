# coauthorship

This repo contains code to reproduce an analysis of coauthorship trends in biomedical research.

To reproduce the analysis, first obtain the preprocessed datasets `pmDat.RData` (from [http://www.ncbi.nlm.nih.gov/pubmed/](PubMed)), `wosDat.RData` (from [Web of Science](https://jcr.incites.thomsonreuters.com/)), and `sjrDat.RData` (from [SCImago](http://www.scimagojr.com/journalrank.php)), which will be made available at a data-sharing repository, and place them in a directory titled `calc` in the root directory. Then execute the sequence of scripts in the file `coauthor.r`.
