import os, re, time, json, openai, pypdf, logging
import pandas as pd, numpy as np, google.generativeai as genai, typing_extensions as typing
from google.api_core.exceptions import GoogleAPICallError
from ollama import chat, ps, ChatResponse, ProcessResponse, ResponseError
from pydantic import BaseModel
from tqdm import tqdm

# Configure logging to write to a file
logging.basicConfig(
    filename='../logs/ollama_logs.txt',
    level=logging.DEBUG,
    format='%(asctime)s - %(levelname)s - %(message)s\n',
    filemode='w'
)



def extract_text_from_pdf(file_path):
    try:
        reader = pypdf.PdfReader(file_path)
        text = ""
        for page in reader.pages:
            text += page.extract_text()
        return text
    except Exception as e:
        print(f"Error encountered while opening or processing {file_path}: {e}")
        return None

def get_all_file_paths(directory):
    file_paths = []
    for root, directories, files in os.walk(directory):
        for filename in files:
            filepath = os.path.join(root, filename)
            file_paths.append(filepath)
    return file_paths



# ChatGPT
def get_gpt_response(user_prompt, system_prompt,engine, max_completion=800,token_buffer=0, retries=3):
    for attempt in range(retries):
        try:
            response = openai.chat.completions.create(
                model=engine,
                messages = [
                    {"role":"system","content": system_prompt},
                    {"role":"user","content": user_prompt}],
                temperature=0.7,
                max_completion_tokens=max_completion,
                top_p=0.95,
                frequency_penalty=0,
                presence_penalty=0,
                stop=None
            )
            #Extract the reply
            reply=response['choices'][0]['message']['content']
            return reply
        except openai.error.InvalidRequestError as e:
            # Increment buffer and retry if invalid request error occurs
            token_buffer += 10
            print(f"Buffer: {token_buffer}, Retry attempt {attempt + 1}")
        except (openai.error.Timeout, openai.error.APIConnectionError) as e:
            return f"Error: {str(e)}"
        except KeyError as e:
            return np.nan
    
    # If all retries fail, return a failure message
    return "Error: Unable to get a valid response after retries."

def count_tokens(text, engine):
    if pd.isna(text):
        return np.nan
    else:
        response = openai.chat.completions.create(
            model=engine,
            messages=[{"role": "user", "content": text}],
            max_completion_tokens=1  # We don't want to generate a response, just count tokens
        )
        return response['usage']['total_tokens']

#function to limit the input tokens
def get_tokens_between_indices(text, engine, max_tokens):
    # Truncate the text based on the max_tokens
    token_count = count_tokens(text,engine)
    
    if token_count > max_tokens:
        # If the token count is greater than the max allowed, truncate the text
        truncated_text = text[:max_tokens]  # Slice the text to fit the limit
        return truncated_text
    else:
        # Return the text as is if it's within the limit
        return text



# Google Gemini
class GeminiSchema(typing.TypedDict):
        Title: str
        Authors: list[str]

def get_gemini_response(user_prompt, api_key)-> str:
    try:
        # Configure the API key
        genai.configure(api_key=api_key)

        # Specify the model you want to use (e.g., "gemini-1.5-flash")
        model = genai.GenerativeModel("gemini-1.5-flash")

        # Generate text
        response = model.generate_content(contents=user_prompt,
                                          generation_config=genai.GenerationConfig(response_mime_type="application/json", response_schema=list[GeminiSchema]),
                                          )
        #print(response)
        return response.text
    
    except GoogleAPICallError as e:
        return f"Google API Error: {e}"
    except Exception as e:
        return f"An unexpected error occurred: {e}"



# Llama 3.1
class LlamaSchema(BaseModel):
    Title: str
    Authors: list[str]

def get_llama_response(user_prompt) -> str:
    try:
        response: ChatResponse = chat(model='llama3.1', 
                        messages=[{'role': 'user',
                                   'content': user_prompt,},],
                        options={'temperature': 0.7}, format=LlamaSchema.model_json_schema(),)
        ans = LlamaSchema.model_validate_json(response.message.content)
        #print(ans)
        return response.message.content # access fields directly from the response object
    except ResponseError as e:
        print('Error:', e.error)



class TitleAuthorExperiment():
    def __init__(self, user_prompt: str, keys: dict,):
        # initialize attributes of an instance
        self.user_prompt=user_prompt
        self.keys=keys
        self.resultpath={"LlamaResult":os.path.join(self.keys['rootdir'], "results", "llama_results2.json"),
                         "GeminiResult":os.path.join(self.keys['rootdir'], "results", "gemini_results2.json")}
        self.llama_results=[]
        self.gemini_results=[]
    
    # methods of an instance
    def clean_text(self, text):
        # Remove invalid or non-printable characters, but keep Unicode printable ones
        return re.sub(r'[^\x20-\x7E\xA0-\uFFFF]', '', text)

    def getEachRes(self,row):
            input_max_tokens = 1000
            raw_text = self.user_prompt.format(row['Content'][0:input_max_tokens])
            return raw_text

    def GetTitleAuthor_gpt(self, metadata):
        engine="gpt-3.5-turbo"
        system_prompt = "Please extract the title and author names:"
        system_prompt_tokens = count_tokens(text=system_prompt, engine=engine)

        def getEachRes(row):
            input_max_tokens = 100

            raw_text = row['Content']
            user_prompt = get_tokens_between_indices(
                text=raw_text, 
                engine=engine, 
                max_tokens=input_max_tokens)
            print(user_prompt)
            
            response=get_gpt_response(user_prompt=user_prompt, 
                                                system_prompt=system_prompt,
                                                engine=engine,
                                                max_completion=500)
            print("ChatGPT Response:", response)
            return response

        results = []
        for i in tqdm(range(len(metadata)), desc="Processing rows"):
            row = metadata.iloc[i]
            result = getEachRes(row)
            results.append(result)

        # After all rows are processed, write the accumulated results
        with open("all_results.json", 'w') as f:
            json.dump(results, f)



    def GetTitleAuthor_gemini(self, metadata: pd.DataFrame,):
        for i in tqdm(range(len(metadata)), desc="Processing rows"):
            row = metadata.iloc[i]
            raw_text = self.getEachRes(row)
            result=get_gemini_response(user_prompt=raw_text, api_key=self.keys['gemini_key'])
            #print(result)
            result=json.loads(self.clean_text(result))[0] #this is the dictionary in the json response
            true_res=metadata.iloc[i,][['ID','Primary_Cat','Title','Authors']].rename({'Title': 'TrueTitle', 'Authors': 'TrueAuthors'}).to_dict()
            result.update(true_res)
            self.gemini_results.append(result)
            time.sleep(10) 
        
        # After all rows are processed, write the accumulated results
        with open(self.resultpath['GeminiResult'], 'w') as f:
            json.dump(self.gemini_results, f)



    def GetTitleAuthor_llama(self, metadata: pd.DataFrame,):
        for i in tqdm(range(len(metadata)), desc="Processing rows"):
            row = metadata.iloc[i]
            raw_text = self.getEachRes(row)
            result=get_llama_response(user_prompt=raw_text)
            result=json.loads(self.clean_text(result))
            true_res=metadata.iloc[i,][['ID','Primary_Cat','Title','Authors']].rename({'Title': 'TrueTitle', 'Authors': 'TrueAuthors'}).to_dict()
            result.update(true_res)
            #print(result)
            self.llama_results.append(result)

        # After all rows are processed, write the accumulated results
        with open(self.resultpath['LlamaResult'], 'w') as f:
            json.dump(self.llama_results, f)



