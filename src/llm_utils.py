import os, openai, pypdf, pandas as pd, numpy as np, google.generativeai as genai, typing_extensions as typing
from google.api_core.exceptions import GoogleAPICallError
from ollama import chat, ps, ChatResponse, ProcessResponse, ResponseError
from pydantic import BaseModel



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