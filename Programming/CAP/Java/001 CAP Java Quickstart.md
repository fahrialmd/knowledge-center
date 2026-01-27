#CAP #CAP-JAVA #Node #CDS #Java #Quickstart
# Purpose
Create a best practice and deployment-ready CAP Java project using simple steps
# Reference
1. N/A
# Implementation
## 1. Install Node
https://nodejs.org/en/download
## 2. Install CDS CLI
```BASH
npm add -g @sap/cds-dk
```
## 3. Install SAP Machine
https://sapmachine.io/
## 4. Generate CAP Java project
```BASH
cds init captest --java --java:mvn -DgroupId=customer
```
## 5. Run application
```BASH
mvn cds:watch
```
# Result
![[Pasted image 20260120135401.png]]
