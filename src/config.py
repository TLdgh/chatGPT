from dotenv import load_dotenv
import os

load_dotenv()

def get_all_variables():
    return {
        "rootdir": os.getenv("rootdir"),
        "openai_key": os.getenv("openai_key"),
        "gemini_key": os.getenv("gemini_key"),
        "connection_str": os.getenv("connection_str")
    }