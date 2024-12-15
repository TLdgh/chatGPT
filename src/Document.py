from PyPDF2 import PdfReader 


class Document:
    def __init__(self, path="", startPage=None, endPage=None):
        self.startPage = startPage
        self.endPage = endPage
        self.loadDocument(path)

    def loadDocument(self, path):
        text = ""

        if str(path).endswith("pdf"):
            pdfFileObj = open(path, "rb")
            reader = PdfReader(pdfFileObj)
            numPages = len(reader.pages)

            # check if start and end page are specified. If not read the whole pdf
            if (self.startPage == None) or (type(self.startPage) != type(0)):
                startPage = 0
            else:
                startPage = self.startPage

            if (
                (self.endPage == None)
                or (self.endPage > numPages)
                or (type(self.endPage) != type(0))
            ):
                endPage = numPages
            else:
                endPage = self.endPage

            if (
                startPage < 0
                or endPage > numPages
                or startPage > numPages
                or startPage > endPage
            ):
                raise ValueError("Invalid page specification.")

            for i in range(startPage, endPage):
                pageObj = reader.pages[i]
                text += pageObj.extract_text()  # extract the text of the page
            pdfFileObj.close()
        else:
            pass

        self.text = text
