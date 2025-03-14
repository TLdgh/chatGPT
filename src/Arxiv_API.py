import requests, feedparser, logging
import pandas as pd
import time, random
import json, os
from Document import Document
from dotenv import load_dotenv
from urllib.parse import parse_qs
from tqdm import tqdm

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

load_dotenv()
rootdir=os.getenv("rootdir")

class Arxiv_API:
    def __init__(
        self,
        search=list,
        start_date="2001-01-01",
        end_date="2023-07-01",
        start=0,
        sampling_unit_size=50,
        max_results=3,
        downloadstuff=False,
    ):
        self.search=search
        self.sampling_unit_size=sampling_unit_size
        self.base_url = "http://export.arxiv.org/api/query?"
        self.file_dir = ['/Volumes/Lacie External Hard Drive/LLMSampleData/' + subj for subj in self.search]

        if self.search != []:
            self.query = ["search_query=cat:%s&start=%i&max_results=%i" % (subj+"*", start, max_results) for subj in self.search]
        else:
            print("Must provide a search item.")

        for fdir in self.file_dir:
            if not os.path.exists(fdir):
                # Create the folder
                os.makedirs(fdir)
        
        self.getResponse(downloadstuff)

    def getResponse(self, downloadstuff):  # get the metadata and download pdf if needed
        self.response = list(range(len(self.query)))
        self.metadata = list(range(len(self.query)))
        self.data = []
        dataCol = [
            "ID",
            "PublishDate",
            "Title",
            "Authors",
            "Journal_Ref",
            "Comment",
            "Abstract",
            "Content",
            "Primary_Cat",
            "Category",
            "PDF_link",
            "file_path",
        ]
        total_niter=len(self.query)*200
        
        with tqdm(total=total_niter, desc="Processing") as pbar:
            for j in range(len(self.query)):
                # perfom a request using the base_url and query. This gives Atom code
                queries=requests.get(self.base_url + self.query[j])
                self.response[j] = queries.content

                # parse the response
                self.metadata[j] = feedparser.parse(self.response[j])

                # print feed information
                logging.info(
                    "Feed title of metadata " + str(j) + ": " + self.metadata[j].feed.title
                )
                logging.info(
                    "Feed last update of metadata "
                    + str(j)
                    + ": "
                    + self.metadata[j].feed.updated
                )
                logging.info(
                    "totalResults for query "
                    + str(j)
                    + ": "
                    + self.metadata[j].feed.opensearch_totalresults
                )
                logging.info(
                    "totalResults for query "
                    + str(j)
                    + ": "
                    + self.metadata[j].feed.opensearch_itemsperpage
                )
                logging.info(
                    "totalResults for query "
                    + str(j)
                    + ": "
                    + self.metadata[j].feed.opensearch_startindex
                )

                if downloadstuff:
                    parsed_query = parse_qs(self.query[j])
                    search_query = parsed_query.get('search_query', [''])[0]  # Get the 'search_query' parameter
                    true_category = search_query.split(':')[1].replace("*","") if ':' in search_query else None  # Extract cat part in search

                    self.data += self.DownloadResult(self.metadata[j], j, true_category, pbar)
                    time.sleep(15)

        if downloadstuff:
            self.df = pd.DataFrame([i for i in self.data], columns=dataCol)

            jfile = self.df.to_dict()
            with open(f"/Volumes/Lacie External Hard Drive/LLMSampleData/metadata.json", "w") as outfile:
                json.dump(jfile, outfile)

    def DownloadResult(
        self, datachunk, j: int, true_category: str, pbar: tqdm
    ):  # download pdf if it exists in arxiv and if we don't have it
        # initiate empty data consisting of list variables
        datalist = []
        count=0
        
        # Run through each entry (article) in each metadata, and print out information
        for i, entry in enumerate(datachunk.entries):
            primcat=entry.tags[0]["term"]
            
            logging.info("Generating e-print metadata for Article " + str(entry.id))

            # get the link and download pdf
            for link in entry.links:
                if (
                    link.rel == "related" and link.title == "pdf"
                ):  # check if there's a pdf in arxiv
                    ids = entry.id.split("/")[-1]
                    pdfurl = link.href
                else:
                    continue
                
                if not pdfurl:
                    logging.warning(f"No PDF available for Article ID: {entry.id}")
                    continue
                    
                filename = "Article_%s" % ids + ".pdf"
                filepath = f"{self.file_dir[j]}/{filename}"
                
                if true_category not in primcat:
                    if os.path.isfile(filepath):    
                        os.remove(filepath)
                        logging.info("PDF doesn't belong to the correct category. Deleted.")
                        continue
                    else:
                        continue
                    
                else:
                    if not os.path.isfile(filepath):  # check if the pdf exists
                        logging.info(
                            "Downloading pdf from metadata list %i article %i ---------"
                            % (j, i)
                        )

                        res = requests.get(pdfurl)
                        if res.status_code == 200:
                            with open(filepath, "wb") as f:
                                f.write(res.content)
                            logging.info("PDF file download --- Complete")
                            timeout = random.randrange(15, 25, 1)
                            logging.info("System sleep: " + str(timeout) + " seconds")
                            time.sleep(timeout)
                            count+=1
                        else:
                            logging.info(
                                "Failed to download pdf from metadata list %i article %i."
                                % (j, i)
                            )
                            continue
                    else:
                            logging.info(
                                "PDF from metadata list %i article %i already exists." % (j, i)
                            )
                            count+=1

                datalist.append(self.addParams(entry, pdfurl, ids, filepath))
                pbar.update(1)
            
            if count==self.sampling_unit_size: break
            
        return datalist

    def addParams(
        self, entry, pdfurl, ids, filepath
    ):  # make the parameters for each article (entry)
        PDF_link = pdfurl
        Id = ids
        file_path = filepath

        try:
            Content = Document(path=file_path, startPage=0, endPage=1).text
        except AttributeError:
            Content = "No Content"

        try:
            PublishDate = entry.published
        except AttributeError:
            PublishDate = "No PublishDate"

        try:
            Title = entry.title
        except AttributeError:
            Title = "No Title"

        try:
            Authors = ", ".join(author.name for author in entry.authors)
        except AttributeError:
            Authors = "No Author"

        try:
            Journal_Ref = entry.arxiv_journal_ref
        except AttributeError:
            Journal_Ref = "No Journal_Ref"

        try:
            Comment = entry.arxiv_comment
        except AttributeError:
            Comment = "No Comment"

        try:
            Abstract = entry.summary
        except AttributeError:
            Abstract = "No Abstract"

        try:
            Primary_Cat = entry.tags[0]["term"]
        except AttributeError:
            Primary_Cat = "No Primary_Cat"

        try:
            Category = [t["term"] for t in entry.tages]
        except AttributeError:
            Category = "No Category"

        res = [
            Id,
            PublishDate,
            Title,
            Authors,
            Journal_Ref,
            Comment,
            Abstract,
            Content,
            Primary_Cat,
            Category,
            PDF_link,
            file_path,
        ]

        return res
