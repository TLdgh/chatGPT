import sys, os, pytest
from dotenv import load_dotenv
sys.path.append('src')
from llm_utils import get_gemini_response


# Load env variables
load_dotenv()
api_key = os.getenv("gemini_key")



def test_api_conn():
    response=get_gemini_response(user_prompt="What is the capital of France?", api_key=api_key)
    
    assert response==f"Paris\n", f"Gemini connection failed with an error: {response}"