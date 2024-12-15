import os
import openai
import pandas as pd
import numpy as np
from PyPDF2 import PdfReader

def extract_text_from_pdf(file_path):
    try:
        reader = PdfReader(file_path)
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





