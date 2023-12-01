# Tunnel Vision</br>

### A Text Analysis on Public Opinions of Stonehenge Road tunnel project (Stonehenge A303 Road Improvement Scheme)</br>
</br>
<div align="justify">

The original work from which this code was taken was submitted in partial fulfilment of the requirements for my MSc Computational Archaeology: GIS, Data Science, and Complexity Degree at UCL. It involved qualitative and quantitative textual analysis of public reactions and opinions data harvested from various mediums on the disputed A303 Stonehenge Road tunnel project. The retrieved data, from all different sources and types, were analysed using the R Studio programming environment, with statistical modelling packages that performed text mining. Inspired by similar works done previously in content analysis and other text mining methods, this work attempted a semi-automated process to analyse the different evidence using natural language processing techniques.</br>
</br>
The study aimed to detect significant patterns of topics within the extracted opinions utilising word frequency, topic modelling, sentiment analysis, n-gram analysis, term frequency-inverse document frequency, etc., with the retrieved results represented with correspondingly compatible visualisations.</br>
</div>

<div align="justify">

### Data:</br>
This study utilised two different public opinion datasets on the Stonehenge Road tunnel. Letters to editors' data are written responses by the public to newspapers in response to news or articles published on the road tunnel and public representations during National Highways consultations. Whilst the public representations are relatively new, the letters to editors have data from 1994 to 2022.</br>
</br>
The public opinion data were hosted in National Infrastructure Planning's Highways: A303 Stonehenge's ['Relevant representation'](https://infrastructure.planninginspectorate.gov.uk/projects/south-west/a303-stonehenge/?ipcsection=relreps&ipcpagesizesubmit=Apply&ipcsearch=&ipcpagesize=100&ipcpage=1) page. Meanwhile, the letters to editors were initially found in a repository of Stonehenge news articles in [Gale Academic OneFile](https://go.gale.com/ps/dispBasicSearch.do?userGroupName=ucl_ttda&prodId=AONE).</br>
</div>

<div align="justify">

### Extraction of data:</br>
The public representation data were sourced by web-scraping through the R package ``rvest``. Two scrapers were developed, one to fetch the name of the person who made the representation and another to retrieve the opinion. The name had to be scrapped since all the opinions had the contributor's name embedded, so the names were demanded in a separate column to filter them from the main synopsis in Excel. The names and the original scrapped output <i>.csv</i> file with opinions with names were then securely deleted to maintain the anonymity of the dataset.</br>
</br>
The scrapper was designed to iterate through all 48 pages of the content, sourcing from the targeted ``href`` and ``a`` tags and writing the retrieved results into a data frame, which would then be outputted into a <i>.csv</i>. The letters to editors were straightforward - the downloaded PDFs were pdf-scrapped through the ``pdftools`` package, loaded into a variable and then fed to a Corpus. Both datasets were extensively cleaned after being scraped and passed into a <i>.csv</i> file.</br>
</div>

### Run order:</br>

</br>

````
webscrapper.R -> pdfscrapper.R -> csvcleaner.R -> sentiment_analysis.R -> analysis.R
````

</br>
