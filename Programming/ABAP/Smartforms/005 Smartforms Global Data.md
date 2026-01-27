# Purpose
Global Data are variables you declare that can be used throughout the entire Smartform.
Key points:
- **Available everywhere**.  Once you define them, you can use these variables in any part of your Smartform (text elements, tables, code, windows, etc.)
- **Hold data temporarily**. They store values while the Smartform is running (like customer name, invoice number, totals)
- **Defined in the Global Data tab**. This is where you declare them (as shown in your screenshot)
- **Types of data**:
	- Simple variables (like strings, numbers, dates)
	- Structure variables (groups of related fields)
	- Internal tables (for lists of data)
	- Constants (fixed values that don't change)

**Simple example**: If you declare a global variable GV_CUSTOMER_NAME, you can display it in the header, footer, or body of your form without declaring it again.
Think of Global Data as a shared storage space that all parts of your Smartform can access and use.
# Reference
- [[001 Smartforms Overview]]
- [[003 Smartforms Start]]
# Implementation
![[Pasted image 20260127093729.png]]
# Result
