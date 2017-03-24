# coauthor

This repo contains code to reproduce the analysis presented in the article [Effects of research complexity and competition on the incidence and growth of coauthorship in biomedicine](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0173444).

To reproduce the analysis:

- Download from Zenodo [the preprocessed dataset `pmDat.RData`](https://zenodo.org/deposit/345934), originally obtained from [http://www.ncbi.nlm.nih.gov/pubmed/](PubMed), and place it in a directory titled `calc` in the root directory.
- Download the needed bibliometric data from [Web of Science](https://jcr.incites.thomsonreuters.com/) and [SCImago](http://www.scimagojr.com/).
- Execute the scripts in the file `coauthor.r` (which may take a while).

To cite the analysis:

Brunson JC, Wang X, Laubenbacher RC (2017) Effects of research complexity and competition on the incidence and growth of coauthorship in biomedicine. *PLoS ONE* **12**(3): e0173444. `doi:10.1371/journal.pone.0173444`