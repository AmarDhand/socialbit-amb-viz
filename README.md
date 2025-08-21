# SocialBit Ambassador — Parse & Visualize
August 21, 2025
Amar Dhand

This R script reads a SocialBit CSV and produces three types of figures:

- **p1**: Daily on-wrist usage vs. social (line plot) with mean lines  
- **p2**: Daily social minutes (bar) with a 380-minute threshold (blue ≥380, pink <380)  
- **p3**: One minute-resolution timeline PNG **per day** (Social vs. No social)

Please note that the upload .csv file should only include one user. This filtering can be done by the download option from socialbit webpage (https://site.socialbitapi.org/).
