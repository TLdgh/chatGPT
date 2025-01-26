import fitz
from PIL import Image
import pytesseract

pytesseract.pytesseract.tesseract_cmd = "/opt/homebrew/bin/tesseract"

class Document:
    def __init__(self, path="", startPage=None, endPage=None):
        self.startPage = startPage
        self.endPage = endPage
        self.loadDocument(path)

    def loadDocument(self, path):
        text = ""

        if str(path).endswith("pdf"):
            pdfDoc = fitz.open(path)
            numPages = pdfDoc.page_count

            startPage = self.startPage if isinstance(self.startPage, int) else 0
            endPage = self.endPage if isinstance(self.endPage, int) and self.endPage <= numPages else numPages

            if startPage < 0 or endPage > numPages or startPage >= endPage:
                raise ValueError("Invalid page specification.")

            for i in range(startPage, endPage):
                page = pdfDoc[i]
                pix = page.get_pixmap(dpi=300)
                img = Image.frombytes("RGB", [pix.width, pix.height], pix.samples)
                text += pytesseract.image_to_string(img) + "\n"

            pdfDoc.close()

        self.text = text.strip()