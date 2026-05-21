import deepdoctection as dd
from matplotlib import pyplot as plt
import csv

print("Initializing Analyzer...")
analyzer = dd.get_dd_analyzer()

pdf_path = "/home/vaibhavagarwal/Downloads/dif_pdf_w3.pdf"
print(f"Processing {pdf_path}...")

df = analyzer.analyze(path=pdf_path)
df.reset_state()

# Fast-forward to Page 4
doc = iter(df)
for _ in range(4):
    page = next(doc) 

# --- NEW EXTRACT & SAVE LOGIC ---
if page.tables:
    print(f"\nFound {len(page.tables)} table(s) on Page 4!")
    
    for idx, table in enumerate(page.tables):
        table_data = table.csv
        
        # 1. Print to terminal
        print(f"\n--- Table {idx + 1} ---")
        for row in table_data:
            print(row)
            
        # 2. Save to CSV file
        csv_filename = f"page_4_table_{idx + 1}.csv"
        with open(csv_filename, "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerows(table_data)
        
        print(f"\n[Saved Table {idx + 1} to {csv_filename}]")
else:
    print("\nNo tables detected on this page.")
# --------------------------------

print("\nRendering...")
image = page.viz(show_figures=True, show_residual_layouts=True)

plt.figure(figsize=(25, 17))
plt.axis('off')
plt.imshow(image)
plt.show()