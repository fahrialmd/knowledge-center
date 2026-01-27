# Purpose
- Creating a smartform
# Reference
- [[001 Smartforms Overview]]
- [[002 Smartforms Smartstyle]]
# Implementation
## 1. Creating a Smartform
![[Pasted image 20260126133651.png]]
## 2. Field List
Field list is a helper feature to insert a variable/parameter into the form
![[Pasted image 20260126133820.png]]
## 3. Form Painter
Form painter is a visual editor to edit the layout of a form
![[Pasted image 20260126134035.png]]
## 4. Global Settings
Global settings control the basic properties of the form, allowing data declaration and logic that is accessible across the entire form with the formatting managed via Smart Styles.
![[Pasted image 20260126134128.png]]
### 4a. Form Attributes
Consists of **General Attributes** and **Output Options**.
In **Output Options**, modify page format, characters per inch, lines per inch, and/or style as needed.
- **Page Format** for the paper size of Smart Form. For example, DINA4 is A4 paper.
- **Characters per Inch (CPI)** specifies how many characters will fit within one inch of horizontal space on a printed document.
- **Lines per Inch (LPI)** indicates the number of text lines printed within one inch of vertical space.
- **Style** to set predefined formatting settings for text elements. Insert [[002 Smartforms Smartstyle]] here.
- **Output Format** specifies the type of document output, determining how the form is rendered and displayed.
![[Pasted image 20260126134258.png]]
### 4b. Form Interface
Consists of parameters for the Smart Form function module: import, export, tables, and exceptions. The interface parameters determine the data to be processed within the form. The parameters would be used in the program logic.
![[Pasted image 20260126135424.png]]
### 4c. Global Definition
Consists of global data, types, field symbols, initialization, form routines, and currency/quantity fields. This section refers to settings and definitions that are accessible throughout the entire form, allowing to define and manage variables, data types, and logic that will be used across different parts of the form.
- Global Data
![[Pasted image 20260126135537.png]]
- Types
Data type definitions for variables and structures used within the form.
For this example, `TY_TNC` type is declared as structure with a table type `TT_TNC` defined.
![[Pasted image 20260127091242.png]]
- Field Symbols
![[Pasted image 20260127091256.png]]
- Initialization
Defines logic that runs before any other processing within the form.
![[Pasted image 20260127091304.png]]
- Form Routines
![[Pasted image 20260127091356.png]]
- Currency/Quant. Fields
![[Pasted image 20260127091403.png]]
# Result
