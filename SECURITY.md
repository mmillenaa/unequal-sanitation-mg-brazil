# Security Policy

## Context
This repository provides replication data, microdata worksheets, and R scripts for academic purposes. It does not host a web application, web server, or collect user data. 

## Data Privacy
The datasets provided are derived from the National Sanitation Information System (SINISA). They consist of institutional and infrastructural aggregate data at the municipal level and do not contain any Personally Identifiable Information (PII) or sensitive personal data.

## Security Risks & Safe Execution
The realistic security risks involve the local execution of R scripts on the user's machine or potential spreadsheet vulnerabilities. 
* All R scripts (`.R`) are plain text and can be inspected before execution.
* Data files (`.csv`, `.xlsx`) are provided strictly as raw data containers without executable macros. Users are encouraged to verify this before opening them in spreadsheet software.

## Reporting a Vulnerability or Data Issue
If you discover a security issue (e.g., a malicious dependency in the R scripts) or an unintended inclusion of sensitive personal data, please do not open a public issue. 

Report it privately by emailing: millena@usp.br. 

Please include the affected file and a brief description of the concern. You will receive an acknowledgment within ten working days.
