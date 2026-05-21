import os
from openai import OpenAI

client = OpenAI(api_key=os.environ.get("OPENAI_API_KEY", ""))

def refine_ocr(input_file_path, output_file_path):

    with open(input_file_path, 'r', encoding='utf-8') as file:
        raw_ocr_text = file.read()
    
    system_prompt = (
        "You are an expert document formatting and data extraction assistant. "
        "Your task is to take messy, raw OCR text and reconstruct it into a clean, highly structured Markdown document. "
        "Do not invent or hallucinate information; only fix typos, formatting artifacts, and broken line breaks."
    )

    user_prompt = f"""
I have raw OCR text extracted from a 5-page document. Please clean it up and format it using Markdown according to these page-specific rules:

1. **Page 1 (Title Page):** Identify the main title, subtitle, and any author/date information. Format the main title as a Level 1 Header (#).
2. **Page 2 (Table of Contents):** Reconstruct the TOC into a clean, nested bulleted or numbered list. Ensure page numbers align logically with the sections.
3. **Page 3 (Text Info):** Fix OCR typos, remove hyphenations caused by line breaks, and combine broken sentences into cohesive paragraphs. Use standard Markdown text.
4. **Pages 4 & 5 (Tables):** This is the most important part. Reconstruct the tabular data into proper Markdown tables. Use your understanding of the context to figure out column alignments if the OCR jumbled the spacing.

Here is the raw OCR text:
---
{raw_ocr_text}
---
"""

    print("Sending text to OpenAI for refinement...")
    
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ],
        temperature=0.1
    )

    refined_text = response.choices[0].message.content

    with open(output_file_path, 'w', encoding='utf-8') as file:
        file.write(refined_text)
        
    print(f"Success! Refined text saved to {output_file_path}")

if __name__ == "__main__":
    refine_ocr(input_file_path="Policy_LM_Workshop_Exercise/Week 2/Workshop 3/ocr_text", output_file_path="refined_output2.md")